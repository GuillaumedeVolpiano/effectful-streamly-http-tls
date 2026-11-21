# effectful-streamly-http-tls

An interpreter for the HTTP effect of
[effectful-streamly-http](https://github.com/GuillaumedeVolpiano/effectful-streamly-http)
that speaks in clear and over TLS, through
[streamly-http-tls](https://github.com/GuillaumedeVolpiano/streamly-http-tls).
It is a package apart so that programs that only speak in clear need not
build a TLS stack.

```haskell
import Effectful
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.HTTP
import Effectful.HTTP.TLS

main :: IO ()
main = do
  outcome <- runEff . runErrorNoCallStack @HttpFailure . runHttp SystemCertificates defaultLimits $
    fetch Https (get "example.org" 443 "/")
  print (fmap (.status) outcome)
```

`runHttp` takes the authorities trusted to sign the certificates of the
servers (`SystemCertificates`, or `CertificatesFrom` a file in PEM) and the
limits of time and size. A certificate that is not accepted is a
`TlsFailed`.

## Building

The libraries this package is built on are not on Hackage yet:
`cabal.project` fetches them from GitHub, at their releases. To build
against copies checked out next to this repository instead, pass
`--project-file=cabal.project.siblings`.

Tested with GHC 9.12.4 and effectful 2.7.1.
