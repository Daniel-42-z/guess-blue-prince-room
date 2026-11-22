module Main (main) where

import Control.Monad (unless)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Guesser (GuessField (..), PartOrd (..), PartialOrdering (..), Result (..))
import Room
import RoomList
import System.Exit (exitFailure)
import System.IO (hSetEncoding, stderr, stdout, utf8)

assert :: String -> Bool -> IO ()
assert label ok = unless ok $ do
  putStrLn ("FAIL: " ++ label)
  exitFailure

assertEq :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEq label expected actual =
  unless (expected == actual) $ do
    putStrLn ("FAIL: " ++ label)
    putStrLn ("  expected: " ++ show expected)
    putStrLn ("  actual:   " ++ show actual)
    exitFailure

allSampleGains :: [Gain]
allSampleGains =
  [ Fixed (-2),
    Fixed (-1),
    Fixed 0,
    Fixed 1,
    Fixed 2,
    VarLoss,
    VarGain,
    PerEntryLoss,
    PerEntryGain,
    SpreadGain,
    Mixed
  ]

testPosetAxioms :: IO ()
testPosetAxioms = do
  -- Reflexivity
  sequence_
    [ assert ("reflexivity: " ++ show x) (partCmp x x == PartialOrdering (Just EQ))
      | x <- allSampleGains
    ]
  -- Antisymmetry
  sequence_
    [ assert
        ("antisymmetry: " ++ show x ++ " vs " ++ show y)
        ( case (getCmp (partCmp x y), getCmp (partCmp y x)) of
            (Just EQ, Just EQ) -> x == y
            (Just LT, Just GT) -> True
            (Just GT, Just LT) -> True
            (Nothing, Nothing) -> True
            _ -> False
        )
      | x <- allSampleGains,
        y <- allSampleGains
    ]
  -- Transitivity
  sequence_
    [ assert
        ("transitivity: " ++ show x ++ ", " ++ show y ++ ", " ++ show z)
        ( case (getCmp (partCmp x y), getCmp (partCmp y z)) of
            (Just LT, Just LT) -> getCmp (partCmp x z) == Just LT
            (Just GT, Just GT) -> getCmp (partCmp x z) == Just GT
            _ -> True
        )
      | x <- allSampleGains,
        y <- allSampleGains,
        z <- allSampleGains
    ]

testGainRules :: IO ()
testGainRules = do
  -- Exact matches
  assertEq "Fixed 1 == Fixed 1 check" Correct (check (Fixed 1) (Fixed 1))
  assertEq "Fixed 1 == Fixed 1 cmp" (PartialOrdering (Just EQ)) (partCmp (Fixed 1) (Fixed 1))
  assertEq "SpreadGain == SpreadGain check" Correct (check SpreadGain SpreadGain)
  assertEq "Mixed == Mixed check" Correct (check Mixed Mixed)

  -- Same-sign Fixed ordering
  assertEq "Fixed 8 vs Fixed 2 check" Partial (check (Fixed 8) (Fixed 2))
  assertEq "Fixed 8 vs Fixed 2 cmp" (PartialOrdering (Just GT)) (partCmp (Fixed 8) (Fixed 2))
  assertEq "Fixed 2 vs Fixed 8 cmp" (PartialOrdering (Just LT)) (partCmp (Fixed 2) (Fixed 8))

  -- Same-sign non-fixed incomparable
  assertEq "VarGain vs Fixed 1 check" Partial (check VarGain (Fixed 1))
  assertEq "VarGain vs Fixed 1 cmp" (PartialOrdering Nothing) (partCmp VarGain (Fixed 1))
  assertEq "SpreadGain vs PerEntryGain check" Partial (check SpreadGain PerEntryGain)
  assertEq "SpreadGain vs PerEntryGain cmp" (PartialOrdering Nothing) (partCmp SpreadGain PerEntryGain)
  assertEq "VarLoss vs PerEntryLoss check" Partial (check VarLoss PerEntryLoss)
  assertEq "VarLoss vs PerEntryLoss cmp" (PartialOrdering Nothing) (partCmp VarLoss PerEntryLoss)

  -- Cross-sign comparisons
  assertEq "VarGain vs Fixed 0 check" Wrong (check VarGain (Fixed 0))
  assertEq "VarGain vs Fixed 0 cmp" (PartialOrdering (Just GT)) (partCmp VarGain (Fixed 0))
  assertEq "VarLoss vs Fixed 0 check" Wrong (check VarLoss (Fixed 0))
  assertEq "VarLoss vs Fixed 0 cmp" (PartialOrdering (Just LT)) (partCmp VarLoss (Fixed 0))
  assertEq "Fixed 1 vs PerEntryLoss check" Wrong (check (Fixed 1) PerEntryLoss)
  assertEq "Fixed 1 vs PerEntryLoss cmp" (PartialOrdering (Just GT)) (partCmp (Fixed 1) PerEntryLoss)

  -- Mixed rules
  assertEq "Mixed vs Fixed 8 check" Partial (check Mixed (Fixed 8))
  assertEq "Mixed vs Fixed 8 cmp" (PartialOrdering Nothing) (partCmp Mixed (Fixed 8))
  assertEq "Mixed vs VarLoss check" Partial (check Mixed VarLoss)
  assertEq "Mixed vs VarLoss cmp" (PartialOrdering Nothing) (partCmp Mixed VarLoss)
  assertEq "Mixed vs Fixed 0 check" Wrong (check Mixed (Fixed 0))
  assertEq "Mixed vs Fixed 0 cmp" (PartialOrdering Nothing) (partCmp Mixed (Fixed 0))

testRoomScenarios :: IO ()
testRoomScenarios = do
  let resClockVsStore = checkResources (resources clockTower) (resources storeRoom)
  assertEq
    "clockTower vs storeRoom (Keys)"
    (Just (Fixed 1, Partial, PartialOrdering Nothing))
    (Map.lookup Keys resClockVsStore)
  assertEq
    "clockTower vs storeRoom (Coins)"
    (Just (Fixed 1, Wrong, PartialOrdering (Just LT)))
    (Map.lookup Coins resClockVsStore)

  let resBedroomVsWeight = checkResources (resources bedroom) (resources weightRoom)
  assertEq
    "bedroom vs weightRoom (Steps)"
    (Just (VarLoss, Wrong, PartialOrdering (Just GT)))
    (Map.lookup Steps resBedroomVsWeight)

  let resAtticVsFoundation = checkResources (resources attic) (resources foundation)
  assertEq
    "attic vs foundation (Other)"
    (Just (Fixed 0, Wrong, PartialOrdering (Just GT)))
    (Map.lookup Other resAtticVsFoundation)

  let resLostVsAttic = checkResources (resources lostAndFound) (resources attic)
  assertEq
    "lostAndFound vs attic (Other)"
    (Just (Fixed 8, Partial, PartialOrdering Nothing))
    (Map.lookup Other resLostVsAttic)

  let resPatioVsMorning = checkResources (resources patio) (resources morningRoom)
  assertEq
    "patio vs morningRoom (Gems)"
    (Just (Fixed 2, Partial, PartialOrdering Nothing))
    (Map.lookup Gems resPatioVsMorning)

  let resBilliardVsClock = checkResources (resources billiardRoom) (resources clockTower)
  assertEq
    "billiardRoom vs clockTower (Keys)"
    (Just (VarGain, Partial, PartialOrdering Nothing))
    (Map.lookup Keys resBilliardVsClock)

  let resChapelVsStore = checkResources (resources chapel) (resources storeRoom)
  assertEq
    "chapel vs storeRoom (Coins)"
    (Just (Fixed 1, Wrong, PartialOrdering (Just LT)))
    (Map.lookup Coins resChapelVsStore)

testSetPartialMatch :: IO ()
testSetPartialMatch = do
  let s1 = Set.fromList [Blue, Red]
      s2 = Set.fromList [Blue, GreenRoom]
      s3 = Set.fromList [Shop, Hallway]
  assertEq "Set exact match" Correct (check s1 s1)
  assertEq "Set overlapping match" Partial (check s1 s2)
  assertEq "Set disjoint match" Wrong (check s1 s3)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  testPosetAxioms
  testGainRules
  testRoomScenarios
  testSetPartialMatch
  putStrLn "All tests passed."
