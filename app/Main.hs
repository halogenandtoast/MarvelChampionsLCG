{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens
import Data.HashMap.Strict (keys)
import Marvel.Entity
import Marvel.Hero
import Marvel.Id
import Marvel.Message
import Marvel.Query
import Relude

modifyingM :: MonadState s m => LensLike m s s a b -> (a -> m b) -> m ()
modifyingM l f = get >>= l f >>= put

instance GameBackend Game m where
  select f = filterM (match f) . every =<< ask

data Game = Game
  { gameHeroes :: HashMap HeroId Hero
  , gamePhase :: Phase
  }
  deriving stock Show

data Phase = HeroPhase | VillainPhase
  deriving stock Show

instance GameEntity Game HeroId where
  every = keys <$> asks gameHeroes
  match m _ = case m of
    AnyHero -> pure True

newtype GameT m a = GameT { unGameT :: ReaderT Game m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Game)

runGameT :: MonadIO m => Game -> GameT m a -> m a
runGameT g = flip runReaderT g . unGameT

newGame :: Game
newGame =
  Game { gameHeroes = fromList [("00001", SpiderMan)], gamePhase = HeroPhase }

heroesL :: Lens' Game (HashMap HeroId Hero)
heroesL = lens gameHeroes $ \m x -> m { gameHeroes = x }

instance RunMessage Game Game where
  runMessage msg = do
    modifyingM heroesL $ traverse (execStateT (runMessage msg))

main :: IO ()
main = runGameT newGame $ do
  result <- execStateT (runMessage Message) =<< ask
  print result
