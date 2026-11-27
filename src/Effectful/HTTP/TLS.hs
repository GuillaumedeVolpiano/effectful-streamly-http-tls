-- |
-- Module      : Effectful.HTTP.TLS
-- Description : The HTTP effect, in clear and over TLS
-- License     : BSD-3-Clause
--
-- An interpreter for the effect of "Effectful.HTTP" that speaks in clear
-- and over TLS.
--
-- > import Effectful
-- > import Effectful.Error.Static (runErrorNoCallStack)
-- > import Effectful.HTTP
-- > import Effectful.HTTP.TLS
-- >
-- > main :: IO ()
-- > main = do
-- >   outcome <- runEff . runErrorNoCallStack @HttpFailure . runHttp SystemCertificates defaultLimits $
-- >     fetch Https (get "example.org" 443 "/")
-- >   print (fmap (.status) outcome)
module Effectful.HTTP.TLS
  ( runHttp
  , Certificates (..)
  ) where

import Control.Exception (IOException, displayException, try)
import Data.ByteString qualified as BS (concat)
import Data.Text (pack)
import Effectful (Eff, IOE, MonadIO (liftIO), UnliftStrategy (SeqUnlift),
                  withEffToIO, type (:>))
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.Error.Static (Error, throwError)
import Effectful.HTTP (Http (..), HttpError, HttpFailure (..), Limits,
                       Response (..))
import Network.TLS (TLSException)
import Streamly.Data.Stream (toList)
import Streamly.External.ByteString (fromArray)
import Streamly.External.Network.HTTP (Client, withResponseFrom)
import Streamly.External.Network.HTTP.TLS (withSecureClient)
import Streamly.External.Network.TLS (Certificates (..))

-- | Interpret the effect over connections in clear and over TLS, within
-- limits of time and size. The connections are kept from one request to
-- the next for each server, as long as the interpreter runs, which over
-- TLS spares a handshake each time; they are closed when it is done. The
-- servers reached over TLS must present a certificate signed by one of
-- the authorities trusted; for them the time to connect includes the TLS
-- handshake.
runHttp :: (IOE :> es, Error HttpFailure :> es) => Certificates -> Limits -> Eff (Http : es) a -> Eff es a
runHttp trusted limits action =
  withEffToIO SeqUnlift $ \runInIO ->
    withSecureClient trusted limits (\client -> runInIO (through client action))

through :: (IOE :> es, Error HttpFailure :> es) => Client -> Eff (Http : es) a -> Eff es a
through client = interpret_ $ \case
  Fetch scheme request ->
    liftIO (try (try (try (withResponseFrom client scheme request whole)))) >>= \case
      Left (problem :: IOException) -> throwError (ConnectFailed (pack (displayException problem)))
      Right (Left (problem :: TLSException)) -> throwError (TlsFailed (pack (displayException problem)))
      Right (Right (Left (problem :: HttpError))) -> throwError (RequestFailed problem)
      Right (Right (Right response)) -> pure response
  where
    whole response = do
      arrays <- toList response.body
      pure (BS.concat (map fromArray arrays) <$ response)
