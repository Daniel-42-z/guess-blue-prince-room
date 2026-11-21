module Guesser where

import Data.Set (Set)
import Data.Set qualified as Set

data Result = Wrong | Partial | Correct deriving (Eq, Ord)

instance Show Result where
  show res = case res of
    Wrong -> "🟥"
    Partial -> "🟨"
    Correct -> "🟦"

class (Eq a) => GuessField a where
  check :: a -> a -> Result
  check ans guess = case guess == ans of
    True -> Correct
    False -> Wrong

instance GuessField Int

instance (Eq a, Ord a) => GuessField (Set a) where
  check ans guess
    | ans == guess = Correct
    | not (Set.null (Set.intersection ans guess)) = Partial
    | otherwise = Wrong

newtype PartialOrdering = PartialOrdering {getCmp :: Maybe Ordering} deriving (Eq, Ord)

instance Show PartialOrdering where
  show ord = case getCmp ord of
    Just LT -> "↓"
    Just GT -> "↑"
    _ -> "-"

-- | Partial order comparison following the same argument order as 'check':
-- @partCmp ans guess@ returns @GT@ (@↑@) when @ans > guess@.
class PartOrd a where
  partCmp :: a -> a -> PartialOrdering

newtype TotalOrd a = TotalOrd a

instance (Ord a) => PartOrd (TotalOrd a) where
  partCmp (TotalOrd x) (TotalOrd y) = PartialOrdering $ Just $ compare x y
