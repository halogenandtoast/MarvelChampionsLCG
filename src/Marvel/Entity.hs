module Marvel.Entity where

import Marvel.Id
import Marvel.Message
import Marvel.Prelude
import Marvel.Query

class RunMessage env a where
  runMessage
    :: ( MonadState a m
       , MonadReader env m
       , GameEntity env HeroId
       , GameBackend env m
       , MonadIO m
       )
    => Message
    -> m ()
