-- | The interpreter against two servers on the loopback interface: one in
-- clear, one over TLS with the certificate of the fixtures.
module Main (main) where

import Control.Concurrent.Async (withAsync)
import Control.Exception (IOException, bracket, catch)
import Control.Monad (forever)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LazyByteString
import Effectful
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.HTTP
import Effectful.HTTP.TLS
import Network.Socket qualified as Net
import Network.Socket.ByteString qualified as NetBS
import Network.TLS qualified as TLS
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
  credential <- TLS.credentialLoadX509 certificateFile keyFile >>= either fail pure
  let shared = (TLS.serverShared TLS.defaultParamsServer) {TLS.sharedCredentials = TLS.Credentials [credential]}
      parameters = TLS.defaultParamsServer {TLS.serverShared = shared}
  bracket listening Net.close $ \plainListener ->
    bracket listening Net.close $ \secureListener -> do
      plainPort <- fromIntegral <$> Net.socketPort plainListener
      securePort <- fromIntegral <$> Net.socketPort secureListener
      let servePlain = forever . bracket (fst <$> Net.accept plainListener) Net.close $ \peer -> do
            request <- readHead (NetBS.recv peer 4096) ""
            NetBS.sendAll peer (answer (BS.takeWhile (/= 13) request))
          serveSecure = forever . bracket (fst <$> Net.accept secureListener) Net.close $ \peer ->
            -- A client that refuses the certificate ends the handshake.
            -- Only these failures are passed over: catching everything
            -- would swallow the cancellation that stops the server.
            quietly . bracket (TLS.contextNew peer parameters) TLS.contextClose $ \context -> do
              TLS.handshake context
              request <- readHead (TLS.recvData context) ""
              TLS.sendData context (LazyByteString.fromStrict (answer (BS.takeWhile (/= 13) request)))
              TLS.bye context
      withAsync servePlain $ \_ -> withAsync serveSecure $ \_ -> run plainPort securePort
  where
    listening = do
      listener <- Net.socket Net.AF_INET Net.Stream Net.defaultProtocol
      Net.setSocketOption listener Net.ReuseAddr 1
      Net.bind listener (Net.SockAddrInet 0 (Net.tupleToHostAddress (127, 0, 0, 1)))
      Net.listen listener 5
      pure listener
    quietly action = action `catch` (\(_ :: TLS.TLSException) -> pure ()) `catch` (\(_ :: IOException) -> pure ())
    readHead receive seen
      | "\r\n\r\n" `BS.isInfixOf` seen = pure seen
      | otherwise = do
          more <- receive
          if BS.null more then pure seen else readHead receive (seen <> more)

echoing :: ByteString -> ByteString
echoing line = "HTTP/1.1 200 OK\r\nContent-Length: " <> lengthOf line <> "\r\n\r\n" <> line
  where
    lengthOf = BS.pack . map (fromIntegral . fromEnum) . show . BS.length

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
          port <- bracket (Net.socket Net.AF_INET Net.Stream Net.defaultProtocol) Net.close $ \socket -> do
            Net.bind socket (Net.SockAddrInet 0 (Net.tupleToHostAddress (127, 0, 0, 1)))
            fromIntegral <$> Net.socketPort socket
          outcome <- with local defaultLimits (fetch Https (get "localhost" port "/"))
          case outcome of
            Left (ConnectFailed _) -> pure ()
            other -> assertFailure ("expected ConnectFailed, got " <> show (fmap (.status) other))
      ]
