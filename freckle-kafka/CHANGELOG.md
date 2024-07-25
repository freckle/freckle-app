## [_Unreleased_](https://github.com/freckle/freckle-app/compare/freckle-kafka-v0.0.0.0...main)

## [v0.0.0.0](https://github.com/freckle/freckle-app/tree/freckle-kafka-v0.0.0.0)

First release, sprouted from `freckle-app-1.18.2.0`.

Changes from `freckle-app`:

- `produceKeyedOnAsync` has been removed; you may substitute the definition
  `(\prTopic values -> void . async . produceKeyedOn prTopic values)`.

- `runConsumer` has been altered; you may recover the original behavior by
  changing `runConsumer pollTimeout onMessage` to
  `withTraceContext $ immortalCreateLogged $ runConsumer pollTimeout onMessage`.
