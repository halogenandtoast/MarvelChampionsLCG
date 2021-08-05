module Marvel.Hero where

import Marvel.Prelude

import Marvel.Entity
import Marvel.Query

data Hero = SpiderMan | CaptainMarvel
  deriving stock Show

instance RunMessage env Hero where
  runMessage _ = do
    heroes <- select AnyHero
    print heroes
