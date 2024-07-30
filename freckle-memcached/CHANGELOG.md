## [_Unreleased_](https://github.com/freckle/freckle-app/compare/freckle-http-v0.0.0.0...main)

## [v0.0.0.0](https://github.com/freckle/freckle-app/tree/freckle-http-v0.0.0.0/freckle-http)

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
