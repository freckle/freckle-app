## [_Unreleased_](https://github.com/freckle/freckle-app/compare/freckle-exception-v0.1.0.0...main)

## [v0.1.0.0](https://github.com/freckle/freckle-app/compare/freckle-exception-v0.0.0.0...freckle-exception-v0.1.0.0)

Adjustment to the behavior of `catchJust`, `tryJust`, and `withException`:
Previously, if any of these utilities would catch and then rethrow an exception,
the exception would always be wrapped in `AnnotatedException` when rethrown.
Now any rethrown exceptions are rethrown exactly as they were caught.

The `HasCallStack` constraint has been removed from all of these functions, as they
now never construct or modify any exceptions and therefore have no reason to access
the call stack.

## [v0.0.0.0](https://github.com/freckle/freckle-app/tree/freckle-exception-v0.0.0.0/freckle-exception)

First release, sprouted from `freckle-app-1.20.0.1`.
