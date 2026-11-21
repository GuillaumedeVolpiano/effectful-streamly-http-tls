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
import Data.ByteString qualified as BS
import Data.Text qualified as Text
import Effectful
import Effectful.Dispatch.Dynamic (interpret_)
import Effectful.Error.Static (Error, throwError)
import Effectful.HTTP (Http (..), HttpError, HttpFailure (..), Limits, Response (..))
import Network.TLS (TLSException)
import Streamly.Data.Stream qualified as Stream
import Streamly.External.ByteString qualified as StreamlyBS
import Streamly.External.Network.HTTP.TLS (withResponseAt)
import Streamly.External.Network.TLS (Certificates (..))

-- | Interpret the effect over connections in clear and over TLS, one for
-- each request, within limits of time and size. The servers reached over
-- TLS must present a certificate signed by one of the authorities
-- trusted; for them the time to connect includes the TLS handshake.
runHttp :: (IOE :> es, Error HttpFailure :> es) => Certificates -> Limits -> Eff (Http : es) a -> Eff es a
runHttp trusted limits = interpret_ $ \case
  Fetch scheme request ->
    liftIO (try (try (try (withResponseAt trusted limits scheme request whole)))) >>= \case
      Left (problem :: IOException) -> throwError (ConnectFailed (Text.pack (displayException problem)))
      Right (Left (problem :: TLSException)) -> throwError (TlsFailed (Text.pack (displayException problem)))
      Right (Right (Left (problem :: HttpError))) -> throwError (RequestFailed problem)
      Right (Right (Right response)) -> pure response
  where
    whole response = do
      arrays <- Stream.toList response.body
      pure (BS.concat (map StreamlyBS.fromArray arrays) <$ response)
