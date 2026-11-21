# Revision history for effectful-streamly-http-tls

## 0.1.0.0 -- unreleased

* `Effectful.HTTP.TLS`: `runHttp`, an interpreter of the `Http` effect in
  clear and over TLS, which keeps its connections, for each server, as
  long as it runs.
