## [_Unreleased_](https://github.com/freckle/freckle-app/compare/v1.0.2.10...main)

## [v1.0.4.0](https://github.com/freckle/freckle-app/compare/v1.0.2.10...v1.0.3.0)

- Add `Freckle.App.Bugsnag` for Bugsnag logging in applications.

## [v1.0.3.0](https://github.com/freckle/freckle-app/compare/v1.0.2.10...v1.0.3.0)

- Add `Freckle.App.Memcache` for using memcached in Apps
- Add `Freckle.App.Scientist` for using [scientist][] in Apps

  [scientist]: https://github.com/freckle/scientist-hs#readme

## [v1.0.2.10](https://github.com/freckle/freckle-app/compare/v1.0.2.9...v1.0.2.10)

- Support GHC 9.0 and 9.2

- Change `Wai` function arguments for producing `RouteName` and `TraceId` to
  tags

  To maintain the same behavior, replace

  ```hs
  makeLoggingMiddleware app getRouteName getTraceId ...
  ```

  With

  ```hs
  makeLoggingMiddleware app getTags ...
    where
      getTags req = catMaybes
        [ ("route", ) <$> getRouteName req
        , ("trace_id", ) <$> getTraceId req
        ]
  ```

  And similar for `makeRequestMetricsMiddleware`.

## [v1.0.2.9](https://github.com/freckle/freckle-app/compare/v1.0.2.8...v1.0.2.9)

- Add some common textual encoding functions to prelude

## [v1.0.2.8](https://github.com/freckle/freckle-app/compare/v1.0.2.7...v1.0.2.8)

- Don't allow `aeson-2.0`

## [v1.0.2.7](https://github.com/freckle/freckle-app/compare/v1.0.2.6...v1.0.2.7)

- Remove explicit `--region` in IAM DB token call
- Relax lower-bounds throughout

## [v1.0.2.6](https://github.com/freckle/freckle-app/compare/v1.0.2.5...v1.0.2.6)

- Add a looser lower-bound on `containers`

## [v1.0.2.5](https://github.com/freckle/freckle-app/compare/v1.0.2.4...v1.0.2.5)

- Add a looser lower-bound on `bytestring`
- Add a looser lower-bound on `template-haskell`

## [v1.0.2.4](https://github.com/freckle/freckle-app/compare/v1.0.2.3...v1.0.2.4)

- Add a looser lower-bound on `base`

## [v1.0.2.3](https://github.com/freckle/freckle-app/compare/v1.0.2.2...v1.0.2.3)

- Add Freckle-specific prelude.

## [v1.0.2.2](https://github.com/freckle/freckle-app/compare/v1.0.2.1...v1.0.2.2)

- Target `hspec-junit-formatter-1.0.3.0` (use `1.1.0.0` in development)

## [v1.0.2.1](https://github.com/freckle/freckle-app/compare/v1.0.2.0...v1.0.2.1)

- Add `denyFrameEmbeddingMiddleware` for denying HTML frame embedding.

## [v1.0.2.0](https://github.com/freckle/freckle-app/compare/v1.0.1.0...v1.0.2.0)

- Add 'Freckle.App.Yesod.Route' to allow printing route names.

## [v1.0.1.0](https://github.com/freckle/freckle-app/compare/v1.0.0.4...v1.0.1.0)

- Added `Freckle.App.Datadog.Gauge` for client side stateful gauges.
- Added `Freckle.App.Datadog.Rts` for sending RTS statistics to DataDog.

## [v1.0.0.4](https://github.com/freckle/freckle-app/compare/v1.0.0.3...v1.0.0.4)

- Support seconds or milliseconds in `PGSTATEMENTTIMEOUT`

  NOTE: We consider this a non-breaking change because the environment variable
  interface is backwards-compatible. By normal Haskell rules, it would be major
  since it's changing the type of something exported.

- Add `respondQueryCanceled` Yesod Middlewares

- Add `makeRequestMetricsMiddleware`

## [v1.0.0.3](https://github.com/freckle/freckle-app/compare/v1.0.0.2...v1.0.0.3)

- Add `package.yaml` to `extra-source-files`.

## [v1.0.0.2](https://github.com/freckle/freckle-app/compare/v1.0.0.1...v1.0.0.2)

- Extract tests that require `git` into a new suite.

## [v1.0.0.1](https://github.com/freckle/freckle-app/compare/v1.0.0.0...v1.0.0.1)

- Ensure `release` GitHub Action completes properly.

## [v1.0.0.0](https://github.com/freckle/freckle-app/tree/v1.0.0.0)

First tagged release.
