# Revision history for effectful-streamly-http-tls

## 0.1.0.0 -- 2026-11-28

* First release on Hackage.
* `Effectful.HTTP.TLS`: `runHttp`, an interpreter of the `Http` effect of
  effectful-streamly-http in clear and over TLS, which keeps its
  connections, for each server, as long as it runs.
