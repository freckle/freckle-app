## [*Unreleased*](https://github.com/freckle/freckle-app/compare/v1.0.0.4...main)

None

## [v1.0.0.4](https://github.com/freckle/freckle-app/compare/v1.0.0.3...v1.0.0.4)

- Support seconds or milliseconds in `PGSTATEMENTTIMEOUT`

  NOTE: We consider this a non-breaking change because the environment variable
  interface is backwards-compatible. By normal Haskell rules, it would be major
  since it's changing the type of something exported.

## [v1.0.0.3](https://github.com/freckle/freckle-app/compare/v1.0.0.2...v1.0.0.3)

- Add `package.yaml` to `extra-source-files`.

## [v1.0.0.2](https://github.com/freckle/freckle-app/compare/v1.0.0.1...v1.0.0.2)

- Extract tests that require `git` into a new suite.

## [v1.0.0.1](https://github.com/freckle/freckle-app/compare/v1.0.0.0...v1.0.0.1)

- Ensure `release` GitHub Action completes properly.

## [v1.0.0.0](https://github.com/freckle/freckle-app/tree/v1.0.0.0)

First tagged release.
