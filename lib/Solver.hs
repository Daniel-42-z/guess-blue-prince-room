module Solver
  ( HintResult (..),
    consistentCandidates,
    bestHint,
    bestHintForCandidates,
  )
where

import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (sortBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import Room (Room, RoomGuessWithResult, guessRoom)
import RoomList (allRooms)

-- | Result returned by the answer-blind optimal solver.
data HintResult = HintResult
  { hintRoom :: Room,
    -- | Expected number of additional guesses from this turn (inclusive).
    hintExpectedGuesses :: Double,
    -- | Guaranteed worst-case additional guesses from this turn (inclusive).
    hintWorstCase :: Int,
    -- | Number of remaining rooms consistent with all previous guesses.
    hintCandidateCount :: Int,
    -- | True if the solver guarantees winning within the remaining guess budget.
    hintGuaranteed :: Bool
  }
  deriving (Eq)

-- | Internal DP solution for a candidate subset.
data Sol = Sol
  { solSum :: !Int,
    solGuess :: !Int,
    solMaxDepth :: !Int
  }
  deriving (Eq)

type MemoTable = Map ([Int], Int) (Maybe Sol)

newtype Memo a = Memo {runMemo :: MemoTable -> (a, MemoTable)}

instance Functor Memo where
  fmap f (Memo m) = Memo $ \s -> let (a, !s') = m s in (f a, s')

instance Applicative Memo where
  pure a = Memo $ \s -> (a, s)
  Memo mf <*> Memo ma = Memo $ \s ->
    let (f, !s1) = mf s
        (a, !s2) = ma s1
     in (f a, s2)

instance Monad Memo where
  Memo ma >>= k = Memo $ \s ->
    let (a, !s1) = ma s
     in runMemo (k a) s1

evalMemo :: Memo a -> MemoTable -> a
evalMemo m s = fst (runMemo m s)

lookupMemo :: ([Int], Int) -> Memo (Maybe (Maybe Sol))
lookupMemo key = Memo $ \s -> (Map.lookup key s, s)

insertMemo :: ([Int], Int) -> Maybe Sol -> Memo ()
insertMemo key val = Memo $ \s -> ((), Map.insert key val s)

numRooms :: Int
numRooms = length allRooms

allIndices :: [Int]
allIndices = [0 .. numRooms - 1]

roomByIndex :: IntMap Room
roomByIndex = IntMap.fromList (zip allIndices allRooms)

-- | Precomputed compact feedback IDs: @feedbackTable ! g ! a@ is the bucket ID
-- for guessing room @g@ when the answer is room @a@.
feedbackTable :: IntMap (IntMap Int)
feedbackTable =
  IntMap.fromList
    [ (gIdx, buildRow gRoom)
      | (gIdx, gRoom) <- zip allIndices allRooms
    ]
  where
    buildRow gRoom =
      let (_, row) =
            foldl'
              ( \(seen, acc) (aIdx, aRoom) ->
                  let key = guessRoom aRoom gRoom
                   in case Map.lookup key seen of
                        Just cid -> (seen, IntMap.insert aIdx cid acc)
                        Nothing ->
                          let cid = Map.size seen
                           in (Map.insert key cid seen, IntMap.insert aIdx cid acc)
              )
              (Map.empty, IntMap.empty)
              (zip allIndices allRooms)
       in row

feedbackRow :: Int -> IntMap Int
feedbackRow gIdx = IntMap.findWithDefault IntMap.empty gIdx feedbackTable

-- | Filter 'allRooms' to those consistent with the observed guess history.
-- Never inspects or requires the hidden answer.
consistentCandidates :: [(Room, RoomGuessWithResult)] -> [Room]
consistentCandidates history =
  [ r
    | r <- allRooms,
      all (\(g, res) -> r /= g && guessRoom r g == res) history
  ]

consistentIndices :: [(Room, RoomGuessWithResult)] -> [Int]
consistentIndices history =
  [ idx
    | (idx, r) <- zip allIndices allRooms,
      all (\(g, res) -> r /= g && guessRoom r g == res) history
  ]

-- | Cached opening hint for the full 110-room set (computed once via exact DP).
cachedOpeningHint :: Maybe HintResult
cachedOpeningHint = solveIndices allIndices 8

-- | Find the optimal next room to guess given the guess history and remaining
-- guess budget (e.g. @maxGuesses - length history@).
-- Minimizes expected additional guesses while guaranteeing a win within the
-- remaining budget whenever possible.
bestHint :: [(Room, RoomGuessWithResult)] -> Int -> Maybe HintResult
bestHint history remainingBudget
  | remainingBudget <= 0 = Nothing
  | null history && remainingBudget >= 4 =
      fmap (\h -> h {hintGuaranteed = hintWorstCase h <= remainingBudget}) cachedOpeningHint
  | otherwise = solveIndices (consistentIndices history) remainingBudget

-- | Find the optimal next room to guess for an explicit list of candidate rooms
-- and remaining guess budget.
bestHintForCandidates :: [Room] -> Int -> Maybe HintResult
bestHintForCandidates cands remainingBudget
  | remainingBudget <= 0 || null cands = Nothing
  | otherwise =
      let candIdxs = [idx | (idx, r) <- zip allIndices allRooms, r `elem` cands]
       in solveIndices candIdxs remainingBudget

solveIndices :: [Int] -> Int -> Maybe HintResult
solveIndices [] _ = Nothing
solveIndices cands@(firstCand : _) remainingBudget =
  let k = length cands
      cappedBudget = min remainingBudget 8
   in case evalMemo (solveBudgeted cands cappedBudget) Map.empty of
        Just sol ->
          Just
            HintResult
              { hintRoom = roomAt (solGuess sol),
                hintExpectedGuesses = fromIntegral (solSum sol) / fromIntegral k,
                hintWorstCase = solMaxDepth sol,
                hintCandidateCount = k,
                hintGuaranteed = solMaxDepth sol <= remainingBudget
              }
        Nothing ->
          -- No guess can guarantee winning within remainingBudget.
          -- If 1 guess remains, pick a remaining candidate to maximize win chance (1/k).
          -- Otherwise fall back to the unconstrained min-expected-guesses policy.
          if remainingBudget == 1
            then
              Just
                HintResult
                  { hintRoom = roomAt firstCand,
                    hintExpectedGuesses = fromIntegral k,
                    hintWorstCase = k,
                    hintCandidateCount = k,
                    hintGuaranteed = False
                  }
            else case evalMemo (solveBudgeted cands 8) Map.empty of
              Just sol ->
                Just
                  HintResult
                    { hintRoom = roomAt (solGuess sol),
                      hintExpectedGuesses = fromIntegral (solSum sol) / fromIntegral k,
                      hintWorstCase = solMaxDepth sol,
                      hintCandidateCount = k,
                      hintGuaranteed = False
                    }
              Nothing -> Nothing

roomAt :: Int -> Room
roomAt idx = case IntMap.lookup idx roomByIndex of
  Just r -> r
  Nothing -> error ("Solver.roomAt: invalid room index " ++ show idx)

bucketLowerBound :: [Int] -> Int
bucketLowerBound b =
  case b of
    [_] -> 1
    _ -> 2 * length b - 1

-- | Partition remaining candidates (excluding @g@) by the feedback produced
-- when guessing @g@. Each bucket preserves ascending index order.
partitionByGuess :: Int -> [Int] -> [[Int]]
partitionByGuess g cands =
  let row = feedbackRow g
      grouped =
        foldr
          ( \a acc ->
              if a == g
                then acc
                else
                  let fid = IntMap.findWithDefault 0 a row
                   in IntMap.insertWith (++) fid [a] acc
          )
          IntMap.empty
          cands
   in IntMap.elems grouped

-- | Exact branch-and-bound DP minimizing total guesses sum subject to
-- @maxDepth <= budget@.
solveBudgeted :: [Int] -> Int -> Memo (Maybe Sol)
solveBudgeted cands budget =
  case cands of
    [] -> pure Nothing
    [x] -> pure (if budget >= 1 then Just (Sol 1 x 1) else Nothing)
    [x, _] -> pure (if budget >= 2 then Just (Sol 3 x 2) else Nothing)
    _
      | budget <= 1 -> pure Nothing
      | otherwise -> do
          let key = (cands, budget)
          cached <- lookupMemo key
          case cached of
            Just res -> pure res
            Nothing -> do
              let k = length cands
                  candSet = IntSet.fromList cands
                  rawMoves =
                    [ (lb, inCandRank, g, sortedBuckets)
                      | g <- allIndices,
                        let inCand = IntSet.member g candSet
                            buckets = partitionByGuess g cands,
                        inCand || length buckets > 1,
                        budget > 2 || all (\b -> length b == 1) buckets,
                        let lb = k + sum (map bucketLowerBound buckets)
                            inCandRank = if inCand then (0 :: Int) else 1
                            sortedBuckets = sortBy (comparing (negate . length)) buckets
                    ]
                  orderedMoves = sortBy (comparing (\(lb, r, g, _) -> (lb, r, g))) rawMoves
              best <- evalMoves k candSet budget Nothing orderedMoves
              insertMemo key best
              pure best

evalMoves ::
  Int ->
  IntSet ->
  Int ->
  Maybe Sol ->
  [(Int, Int, Int, [[Int]])] ->
  Memo (Maybe Sol)
evalMoves _ _ _ best [] = pure best
evalMoves k candSet budget best ((lb, _, g, buckets) : rest) =
  case best of
    Just (Sol bestSum _ _) | lb > bestSum -> pure best
    _ -> do
      let remLbs = map bucketLowerBound buckets
          totalSuffix = sum remLbs
      res <- evalBuckets (budget - 1) (fmap solSum best) k 1 totalSuffix (zip buckets remLbs)
      let best' = case res of
            Nothing -> best
            Just (totSum, maxD) ->
              let candSol = Sol totSum g maxD
               in if isBetter candSet candSol best
                    then Just candSol
                    else best
      evalMoves k candSet budget best' rest

isBetter :: IntSet -> Sol -> Maybe Sol -> Bool
isBetter _ _ Nothing = True
isBetter candSet (Sol s1 g1 d1) (Just (Sol s2 g2 d2)) =
  let r1 = if IntSet.member g1 candSet then (0 :: Int) else 1
      r2 = if IntSet.member g2 candSet then (0 :: Int) else 1
   in (s1, d1, r1, g1) < (s2, d2, r2, g2)

evalBuckets ::
  Int ->
  Maybe Int ->
  Int ->
  Int ->
  Int ->
  [([Int], Int)] ->
  Memo (Maybe (Int, Int))
evalBuckets _ _ accSum accMaxD _ [] = pure (Just (accSum, accMaxD))
evalBuckets subBudget bestSumLimit accSum accMaxD suffixLb ((b, bLb) : rest) = do
  let suffixLb' = suffixLb - bLb
  subRes <- solveBudgeted b subBudget
  case subRes of
    Nothing -> pure Nothing
    Just (Sol subSum _ subD) ->
      let accSum' = accSum + subSum
          accMaxD' = max accMaxD (1 + subD)
       in case bestSumLimit of
            Just lim | accSum' + suffixLb' > lim -> pure Nothing
            _ -> evalBuckets subBudget bestSumLimit accSum' accMaxD' suffixLb' rest
