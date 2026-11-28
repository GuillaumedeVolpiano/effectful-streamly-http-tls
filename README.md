# effectful-streamly-http-tls

An interpreter for the HTTP effect of
[effectful-streamly-http](https://hackage.haskell.org/package/effectful-streamly-http)
that speaks in clear and over TLS, through
[streamly-http-tls](https://hackage.haskell.org/package/streamly-http-tls).
It is a package apart so that programs that only speak in clear need not
build a TLS stack.

```haskell
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

import Effectful (runEff)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.HTTP (HttpFailure, Response (..), Scheme (Https), defaultLimits,
                       fetch, get)
import Effectful.HTTP.TLS (Certificates (SystemCertificates), runHttp)

main :: IO ()
main = do
  outcome <- runEff . runErrorNoCallStack @HttpFailure . runHttp SystemCertificates defaultLimits $
    fetch Https (get "example.org" 443 "/")
  print (fmap (.status) outcome)
```

`runHttp` takes the authorities trusted to sign the certificates of the
servers (`SystemCertificates`, or `CertificatesFrom` a file in PEM) and the
limits of time and size. A certificate that is not accepted is a
`TlsFailed`. The connections are kept from one request to the next for
each server as long as the interpreter runs, which over TLS spares a
handshake each time.

## Building

To build against copies of the libraries this package is built on
([effectful-streamly-http](https://hackage.haskell.org/package/effectful-streamly-http),
[streamly-http-tls](https://hackage.haskell.org/package/streamly-http-tls)
and those under them) checked out next to this repository, pass
`--project-file=cabal.project.siblings`.

Tested with GHC 9.8.4, 9.10.3 and 9.12.4, and effectful 2.7.1.
