## [_Unreleased_](https://github.com/freckle/freckle-app/compare/freckle-http-v0.3.0.0...main)

## [v0.3.0.0](https://github.com/freckle/freckle-app/compare/freckle-http-v0.2.0.0...freckle-http-v0.3.0.0)

- Update `HttpCache.set` to accept TTL (and use it in memcached implementation)

## [v0.2.0.0](https://github.com/freckle/freckle-app/compare/freckle-http-v0.1.0.0...freckle-http-v0.2.0.0)

`MonadHttp.httpLbs` has a `HasCallStack` constraint, and instances throw `AnnotatedException`

Breaking change: `httpStubbed` is now monadic rather than pure. Its errors are thrown into `IO` as
`AnnotatedException`-wrapped `NoStubsMatched`.

## [v0.1.0.0](https://github.com/freckle/freckle-app/compare/freckle-http-v0.0.0.0...freckle-http-v0.1.0.0)

Removes `Freckle.App.HttpSpec` which had been included by mistake.

## [v0.0.0.0](https://github.com/freckle/freckle-app/tree/freckle-http-v0.0.0.0/freckle-http)

First release, sprouted from `freckle-app-1.19.0.0`.
