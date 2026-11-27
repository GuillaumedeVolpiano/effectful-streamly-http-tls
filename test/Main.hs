-- | The interpreter against two servers on the loopback interface: one in
-- clear, one over TLS with the certificate of the fixtures.
module Main (main) where

import Control.Concurrent.Async (withAsync)
import Control.Exception (IOException, bracket, catch)
import Control.Monad (forever)
import Data.ByteString (ByteString, isInfixOf, pack)
import Data.ByteString qualified as BS (length, null, takeWhile)
import Data.ByteString.Lazy (fromStrict)
import Effectful (Eff, IOE, runEff)
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.HTTP (Http, HttpError (BodyTooLarge, ConnectTimedOut),
                       HttpFailure (ConnectFailed, RequestFailed, TlsFailed),
                       Limits (connectTimeout, maxBodySize),
                       Response (body, status), Scheme (Http, Https),
                       defaultLimits, fetch, get)
import Effectful.HTTP.TLS (Certificates (..), runHttp)
import Network.Socket (Family (AF_INET), SockAddr (SockAddrInet),
                       SocketOption (ReuseAddr), SocketType (Stream), accept,
                       bind, close, defaultProtocol, listen, setSocketOption,
                       socketPort, tupleToHostAddress)
import Network.Socket qualified as N (socket)
import Network.Socket.ByteString (recv, sendAll)
import Network.TLS (Credentials (Credentials), ServerParams (serverShared),
                    Shared (sharedCredentials), TLSException, bye, contextClose,
                    contextNew, credentialLoadX509, defaultParamsServer,
                    handshake, recvData, sendData)
import Test.Tasty (TestTree, defaultMain, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

main :: IO ()
main = defaultMain tests

certificateFile, keyFile :: FilePath
certificateFile = "test/fixtures/localhost-cert.pem"
keyFile = "test/fixtures/localhost-key.pem"

-- | Run a server in clear and a server over TLS that answer every
-- connection with a function of the first line of the request. Gives
-- their ports.
withServers :: (ByteString -> ByteString) -> (Int -> Int -> IO a) -> IO a
withServers answer run = do
  credential <- credentialLoadX509 certificateFile keyFile >>= either fail pure
  let shared = (serverShared defaultParamsServer) {sharedCredentials = Credentials [credential]}
      parameters = defaultParamsServer {serverShared = shared}
  bracket listening close $ \plainListener ->
    bracket listening close $ \secureListener -> do
      plainPort <- fromIntegral <$> socketPort plainListener
      securePort <- fromIntegral <$> socketPort secureListener
      let servePlain = forever . bracket (fst <$> accept plainListener) close $ \peer -> do
            request <- readHead (recv peer 4096) ""
            sendAll peer (answer (BS.takeWhile (/= 13) request))
          serveSecure = forever . bracket (fst <$> accept secureListener) close $ \peer ->
            -- A client that refuses the certificate ends the handshake.
            -- Only these failures are passed over: catching everything
            -- would swallow the cancellation that stops the server.
            quietly . bracket (contextNew peer parameters) contextClose $ \context -> do
              handshake context
              request <- readHead (recvData context) ""
              sendData context (fromStrict (answer (BS.takeWhile (/= 13) request)))
              bye context
      withAsync servePlain $ \_ -> withAsync serveSecure $ \_ -> run plainPort securePort
  where
    listening = do
      listener <- N.socket AF_INET Stream defaultProtocol
      setSocketOption listener ReuseAddr 1
      bind listener (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
      listen listener 5
      pure listener
    quietly action = action `catch` (\(_ :: TLSException) -> pure ()) `catch` (\(_ :: IOException) -> pure ())
    readHead receive seen
      | "\r\n\r\n" `isInfixOf` seen = pure seen
      | otherwise = do
          more <- receive
          if BS.null more then pure seen else readHead receive (seen <> more)

echoing :: ByteString -> ByteString
echoing line = "HTTP/1.1 200 OK\r\nContent-Length: " <> lengthOf line <> "\r\n\r\n" <> line
  where
    lengthOf = pack . map (fromIntegral . fromEnum) . show . BS.length

with :: Certificates -> Limits -> Eff '[Http, Error HttpFailure, IOE] a -> IO (Either HttpFailure a)
with trusted limits = runEff . runErrorNoCallStack . runHttp trusted limits

local :: Certificates
local = CertificatesFrom certificateFile

tests :: TestTree
tests =
  localOption (mkTimeout 20_000_000) $
    testGroup
      "effectful-streamly-http-tls"
      [ testCase "requests go in clear and over TLS, as each asks" $
          withServers echoing $ \plainPort securePort -> do
            outcome <- with local defaultLimits $ do
              inClear <- fetch Http (get "127.0.0.1" plainPort "/clear")
              overTls <- fetch Https (get "localhost" securePort "/secure")
              pure (inClear.body, overTls.body)
            outcome @?= Right ("GET /clear HTTP/1.1", "GET /secure HTTP/1.1")
      , testCase "a certificate signed by nobody the system trusts is a TLS failure" $
          withServers echoing $ \_ securePort -> do
            outcome <- with SystemCertificates defaultLimits (fetch Https (get "localhost" securePort "/"))
            case outcome of
              Left (TlsFailed _) -> pure ()
              other -> assertFailure ("expected TlsFailed, got " <> show (fmap (.status) other))
      , testCase "the limits apply over TLS" $
          withServers echoing $ \plainPort securePort -> do
            tooLarge <- with local defaultLimits {maxBodySize = Just 5} (fetch Https (get "localhost" securePort "/"))
            fmap (.status) tooLarge @?= Left (RequestFailed (BodyTooLarge 5))
            -- The server in clear waits for the end of a request while
            -- the client waits for the TLS handshake.
            stuck <- with local defaultLimits {connectTimeout = Just 200_000} (fetch Https (get "localhost" plainPort "/"))
            fmap (.status) stuck @?= Left (RequestFailed ConnectTimedOut)
      , testCase "a refused connection is reported" $ do
          port <- bracket (N.socket AF_INET Stream defaultProtocol) close $ \socket -> do
            bind socket (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
            fromIntegral <$> socketPort socket
          outcome <- with local defaultLimits (fetch Https (get "localhost" port "/"))
          case outcome of
            Left (ConnectFailed _) -> pure ()
            other -> assertFailure ("expected ConnectFailed, got " <> show (fmap (.status) other))
      ]
