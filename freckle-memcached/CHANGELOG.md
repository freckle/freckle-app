## [_Unreleased_](https://github.com/freckle/freckle-app/compare/freckle-memcached-v0.0.0.2...main)

## [v0.0.0.2](https://github.com/freckle/freckle-app/compare/freckle-memcached-v0.0.0.1...freckle-memcached-v0.0.0.2)

Upgrade `Blammo` to 2.1

## [v0.0.0.1](https://github.com/freckle/freckle-app/compare/freckle-memcached-v0.0.0.0...freckle-memcached-v0.0.0.1)

Drop `relude` dependency

## [v0.0.0.0](https://github.com/freckle/freckle-app/tree/freckle-memcached-v0.0.0.0/freckle-memcached)

First release, sprouted from `freckle-app-1.19.0.0`.

A typeclass instance related to Yesod has been removed. To recover the original behavior,
you can add this instance:

```haskell
import Yesod.Core.Types (HandlerData, RunHandlerEnv, handlerEnv, rheSite)

instance HasMemcachedClient site => HasMemcachedClient (HandlerData child site) where
  memcachedClientL = envL . siteL . memcachedClientL

envL :: Lens' (HandlerData child site) (RunHandlerEnv child site)
envL = lens handlerEnv $ \x y -> x {handlerEnv = y}

siteL :: Lens' (RunHandlerEnv child site) site
siteL = lens rheSite $ \x y -> x {rheSite = y}
```
