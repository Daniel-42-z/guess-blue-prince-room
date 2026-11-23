module Room where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Guesser (GuessField (..), PartOrd (partCmp), PartialOrdering (..), Result (..))

data Shape
  = DeadEnd
  | Straight
  | Corner
  | TShaped
  | Crossroad
  deriving (Eq, Ord, Show)

instance GuessField Shape where
  check ans guess
    | ans == guess = Correct
    | otherwise = case (ans, guess) of
        (Straight, Corner) -> Partial
        (Corner, Straight) -> Partial
        _ -> Wrong

data Color = Red | Hallway | Shop | GreenRoom | Blue | Bedroom | Blackprint deriving (Eq, Show, Ord)

instance GuessField Color

type GemCost = Int

data PoolCategory = Starting | Found | Outer | Studio deriving (Eq, Ord, Show)

instance GuessField PoolCategory

data SpecialCategory = None | Drafting | Entry | Mechanical | Permanent | Puzzle | Spread | Tomorrow deriving (Eq, Ord, Show)

instance GuessField SpecialCategory

data Resource = Steps | Coins | Gems | Keys | Other
  deriving (Eq, Ord, Show, Enum, Bounded)

data Gain
  = Fixed Int
  | VarGain
  | VarLoss
  | PerEntryGain
  | PerEntryLoss
  | SpreadGain
  | Purchase -- Only if able to gain this specific resource through making purchases. So coins can only be purchased from laundry room
  | Mixed -- Lost & found
  deriving (Eq, Ord)

instance Show Gain where
  show g = case g of
    Fixed 0 -> "0"
    Fixed n | n > 0 -> "+" ++ show n
    Fixed n -> show n
    VarGain -> "+?"
    VarLoss -> "-?"
    PerEntryGain -> "+/entry"
    PerEntryLoss -> "-/entry"
    SpreadGain -> "+spread"
    Purchase -> "purchase"
    Mixed -> "+/-"

gainSign :: Gain -> Maybe Ordering
gainSign g = case g of
  Fixed n -> Just (compare n 0)
  VarGain -> Just GT
  PerEntryGain -> Just GT
  SpreadGain -> Just GT
  VarLoss -> Just LT
  PerEntryLoss -> Just LT
  Purchase -> Just GT
  Mixed -> Nothing

instance GuessField Gain where
  check ans guess
    | ans == guess = Correct
    | otherwise = case (gainSign ans, gainSign guess) of
        (Just s1, Just s2) | s1 == s2 -> Partial
        (Nothing, Just s) | s /= EQ -> Partial
        (Just s, Nothing) | s /= EQ -> Partial
        _ -> Wrong

instance PartOrd Gain where
  partCmp ans guess
    | ans == guess = PartialOrdering (Just EQ)
    | otherwise = case (ans, guess) of
        (Fixed a, Fixed b) -> PartialOrdering (Just (compare a b))
        _ -> case (gainSign ans, gainSign guess) of
          (Just s1, Just s2) | s1 /= s2 -> PartialOrdering (Just (compare s1 s2))
          _ -> PartialOrdering Nothing

noResourceGain :: Map Resource Gain
noResourceGain = Map.fromList [(r, Fixed 0) | r <- [minBound .. maxBound]]

resourceGains :: [(Resource, Gain)] -> Map Resource Gain
resourceGains overrides = Map.union (Map.fromList overrides) noResourceGain

checkResources ::
  Map Resource Gain ->
  Map Resource Gain ->
  Map Resource (Gain, Result, PartialOrdering)
checkResources ansMap guessMap =
  Map.fromList
    [ (r, (g, check a g, partCmp a g))
    | r <- [minBound .. maxBound],
      let a = Map.findWithDefault (Fixed 0) r ansMap
          g = Map.findWithDefault (Fixed 0) r guessMap
    ]

data Room = Room
  { name :: String,
    shape :: Shape,
    colors :: Set Color,
    gemCost :: GemCost,
    poolCat :: PoolCategory,
    upgradable :: Bool,
    specialCats :: Set SpecialCategory,
    resources :: Map Resource Gain
  }
  deriving (Eq)

data RoomGuessWithResult = RoomGuessWithResult
  { shapeWithResult :: (Shape, Result),
    colorsWithResult :: (Set Color, Result),
    gemCostWithResult :: (GemCost, Result, Ordering),
    poolCatWithResult :: (PoolCategory, Result),
    upgradableWithResult :: (Bool, Result),
    specialCatsWithResult :: (Set SpecialCategory, Result),
    resourcesWithResult :: Map Resource (Gain, Result, PartialOrdering)
  }
  deriving (Eq, Ord)

guessRoom :: Room -> Room -> RoomGuessWithResult
guessRoom ans guess =
  RoomGuessWithResult
    { shapeWithResult = (shape guess, check (shape ans) (shape guess)),
      colorsWithResult = (colors guess, check (colors ans) (colors guess)),
      gemCostWithResult = (gemCost guess, check (gemCost ans) (gemCost guess), compare (gemCost ans) (gemCost guess)),
      poolCatWithResult = (poolCat guess, check (poolCat ans) (poolCat guess)),
      upgradableWithResult = (upgradable guess, check (upgradable ans) (upgradable guess)),
      specialCatsWithResult = (specialCats guess, check (specialCats ans) (specialCats guess)),
      resourcesWithResult = checkResources (resources ans) (resources guess)
    }
