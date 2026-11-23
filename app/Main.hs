{-# LANGUAGE CPP #-}

module Main (main) where

import Control.Exception (IOException, catch)
import Data.Char (isSpace, toLower)
import Data.List (intercalate, isInfixOf)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
#ifdef mingw32_HOST_OS
import Foreign.C.Types (CBool (..), CUInt (..))
#endif
import Guesser (PartialOrdering (..), Result (..))
import Room
import RoomList (allRooms)
import System.CPUTime (getCPUTime)
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hFlush, hPutStrLn, hSetEncoding, stderr, stdin, stdout, utf8)
import Text.Read (readMaybe)

#ifdef mingw32_HOST_OS
foreign import stdcall unsafe "windows.h SetConsoleOutputCP"
  c_SetConsoleOutputCP :: CUInt -> IO CBool

foreign import stdcall unsafe "windows.h SetConsoleCP"
  c_SetConsoleCP :: CUInt -> IO CBool
#endif

-- | Ensure standard handles (and the Windows console code page) use UTF-8 so
-- Unicode glyphs such as colored squares and arrows do not fail to encode.
initUtf8Console :: IO ()
initUtf8Console = do
#ifdef mingw32_HOST_OS
  let utf8CodePage = 65001
  _ <- c_SetConsoleCP utf8CodePage
  _ <- c_SetConsoleOutputCP utf8CodePage
#endif
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

defaultMaxGuesses :: Int
defaultMaxGuesses = 8

newtype Options = Options
  { optMaxGuesses :: Int
  }

defaultOptions :: Options
defaultOptions = Options {optMaxGuesses = defaultMaxGuesses}

optionsSpec :: [OptDescr (Options -> Either String Options)]
optionsSpec =
  [ Option
      ['g']
      ["guesses"]
      ( ReqArg
          ( \arg opts -> case readMaybe arg of
              Just n | n > 0 -> Right opts {optMaxGuesses = n}
              _ -> Left ("invalid guess count: " ++ arg ++ " (expected positive integer)")
          )
          "N"
      )
      ("maximum number of guesses (default: " ++ show defaultMaxGuesses ++ ")"),
    Option
      ['h']
      ["help"]
      (NoArg (\_ -> Left ""))
      "show this help message"
  ]

parseArgs :: [String] -> IO Options
parseArgs args = do
  prog <- getProgName
  let header = "Usage: " ++ prog ++ " [OPTIONS]"
  case getOpt Permute optionsSpec args of
    (actions, [], []) -> case foldl (>>=) (Right defaultOptions) actions of
      Right opts -> pure opts
      Left "" -> do
        putStr (usageInfo header optionsSpec)
        exitSuccess
      Left err -> do
        hPutStrLn stderr ("Error: " ++ err)
        hPutStrLn stderr (usageInfo header optionsSpec)
        exitFailure
    (_, nonOpts, errs) -> do
      let allErrs = errs ++ ["unexpected argument: " ++ a | a <- nonOpts]
      mapM_ (hPutStrLn stderr) allErrs
      hPutStrLn stderr (usageInfo header optionsSpec)
      exitFailure

pickRandomRoom :: [Room] -> IO Room
pickRandomRoom [] = error "allRooms is empty"
pickRandomRoom rooms = do
  t <- getCPUTime
  let mixed = (t `div` 1000) `xorShift` (t `div` 1000000)
      idx = fromIntegral (abs mixed `mod` fromIntegral (length rooms))
  pure (rooms !! idx)
  where
    xorShift a b = a * 6364136223846793005 + b + 1

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
  where
    dropWhileEnd p = reverse . dropWhile p . reverse

normalize :: String -> String
normalize = map toLower . trim

findRoom :: String -> Maybe Room
findRoom input =
  let key = normalize input
   in case filter (\r -> normalize (name r) == key) allRooms of
        (r : _) -> Just r
        [] -> Nothing

suggestRooms :: String -> [String]
suggestRooms input =
  let key = normalize input
   in [name r | r <- allRooms, key `isInfixOf` normalize (name r)]

formatSet :: (Show a) => Set a -> String
formatSet s
  | Set.null s = "None"
  | otherwise = intercalate ", " (map show (Set.toList s))

formatOrdering :: Ordering -> String
formatOrdering ord = case ord of
  LT -> " ↓"
  GT -> " ↑"
  EQ -> ""

formatResourceEntry :: Resource -> (Gain, Result, PartialOrdering) -> String
formatResourceEntry res (gain, result, pOrd) =
  let arrow = case getCmp pOrd of
        Just EQ -> ""
        Nothing -> ""
        Just _ -> " " ++ show pOrd
   in show res ++ ": " ++ show result ++ " " ++ show gain ++ arrow

printGuessResult :: Room -> RoomGuessWithResult -> IO ()
printGuessResult guess res = do
  let (shp, shpRes) = shapeWithResult res
      (cols, colsRes) = colorsWithResult res
      (gems, gemsRes, gemsOrd) = gemCostWithResult res
      (pool, poolRes) = poolCatWithResult res
      (upg, upgRes) = upgradableWithResult res
      (specs, specsRes) = specialCatsWithResult res
      resEntries =
        [ formatResourceEntry r entry
          | r <- [minBound .. maxBound],
            Just entry <- [Map.lookup r (resourcesWithResult res)]
        ]
  putStrLn ("  Room:       " ++ name guess)
  putStrLn ("  Shape:      " ++ show shpRes ++ " " ++ show shp)
  putStrLn ("  Colors:     " ++ show colsRes ++ " " ++ formatSet cols)
  putStrLn ("  Gems:       " ++ show gemsRes ++ " " ++ show gems ++ formatOrdering gemsOrd)
  putStrLn ("  Pool:       " ++ show poolRes ++ " " ++ show pool)
  putStrLn ("  Upgradable: " ++ show upgRes ++ " " ++ if upg then "Yes" else "No")
  putStrLn ("  Special:    " ++ show specsRes ++ " " ++ formatSet specs)
  putStrLn ("  Res:        " ++ intercalate " | " resEntries)

readLineSafe :: IO (Maybe String)
readLineSafe =
  (Just <$> getLine) `catch` \(_ :: IOException) -> pure Nothing

gameLoop :: Room -> Int -> Int -> IO ()
gameLoop ans guessNum maxGuesses
  | guessNum > maxGuesses =
      putStrLn ("Out of guesses! The room was: " ++ name ans)
  | otherwise = do
      putStr ("Guess (" ++ show guessNum ++ "/" ++ show maxGuesses ++ "): ")
      hFlush stdout
      mLine <- readLineSafe
      case mLine of
        Nothing -> putStrLn ("\nExiting. The room was: " ++ name ans)
        Just raw ->
          let input = trim raw
           in case normalize input of
                "" -> gameLoop ans guessNum maxGuesses
                "quit" -> putStrLn ("Bye! The room was: " ++ name ans)
                "exit" -> putStrLn ("Bye! The room was: " ++ name ans)
                "lose" -> putStrLn ("You lose! The room was: " ++ name ans)
                "giveup" -> putStrLn ("You lose! The room was: " ++ name ans)
                "give up" -> putStrLn ("You lose! The room was: " ++ name ans)
                "list" -> do
                  putStrLn ("Rooms (" ++ show (length allRooms) ++ "): " ++ intercalate ", " (map name allRooms))
                  gameLoop ans guessNum maxGuesses
                _ -> case findRoom input of
                  Nothing -> do
                    putStrLn ("Unknown room: " ++ input)
                    case suggestRooms input of
                      [] -> pure ()
                      matches -> putStrLn ("Did you mean: " ++ intercalate ", " (take 5 matches))
                    gameLoop ans guessNum maxGuesses
                  Just guess -> do
                    printGuessResult guess (guessRoom ans guess)
                    if guess == ans
                      then putStrLn ("Correct! You guessed " ++ name ans ++ " in " ++ show guessNum ++ "/" ++ show maxGuesses ++ " guesses.")
                      else gameLoop ans (guessNum + 1) maxGuesses

main :: IO ()
main = do
  initUtf8Console
  opts <- getArgs >>= parseArgs
  ans <- pickRandomRoom allRooms
  putStrLn ("Blue Prince Room Guesser (" ++ show (optMaxGuesses opts) ++ " guesses)")
  putStrLn "Type a room name to guess, 'list' to see all rooms, 'lose' / 'give up' to forfeit, or 'quit' to exit."
  gameLoop ans 1 (optMaxGuesses opts)
