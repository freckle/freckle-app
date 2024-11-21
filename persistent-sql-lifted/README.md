# persistent-sql-lifted

How to migrate from vanilla [persistent]:

- Instead of [SqlPersistT], use a `MonadSqlBackend` constraint.
- Define an instance of `MonadSqlTx` for your application Monad that specifies how
  your application runs database transactions, e.g. by running [runSqlPool].
- Instead of calling `runSqlPool` directly from the rest of your application code,
  use the `runSqlTx` method from the `MonadSqlTx` class.

  [persistent]: https://hackage.haskell.org/package/persistent
  [SqlPersistT]: https://hackage.haskell.org/package/persistent-2.14.6.3/docs/Database-Persist-Sql.html#t:SqlPersistT
  [runSqlPool]: https://hackage.haskell.org/package/persistent-2.14.6.3/docs/Database-Persist-Sql.html#v:runSqlPool
