module Marvel.Id where

import Marvel.Prelude

newtype HeroId = HeroId Text
  deriving newtype (IsString, Show, Eq, Hashable)
