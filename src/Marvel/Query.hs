module Marvel.Query where

import Marvel.Prelude

import Marvel.Id

data HeroFilter = AnyHero

type family Filter val where
  Filter HeroId = HeroFilter

type family FilterResult val where
  FilterResult HeroFilter = HeroId

class GameEntity env val where
  every :: MonadReader env m => m [val]
  match :: MonadReader env m => Filter val -> val -> m Bool

class GameBackend env m where
  select :: (MonadReader env m, GameEntity env val, FilterResult (Filter val) ~ val) => Filter val -> m [val]
