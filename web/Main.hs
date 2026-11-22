{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Char (isSpace, toLower)
import Data.List (intercalate, isInfixOf)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Guesser (PartialOrdering (..), Result (..))
import Miso hiding ((!!))
import Miso.Html.Element as H
import Miso.Html.Event as E
import Miso.Html.Property as P
import Room
import RoomList (allRooms)
import System.Random (randomRIO)

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

defaultMaxGuesses :: Int
defaultMaxGuesses = 8

data GameStatus = Playing | Won | Lost
  deriving (Eq)

data Model = Model
  { mAnswer :: Room,
    mGuesses :: [(Room, RoomGuessWithResult)],
    mInput :: MisoString,
    mError :: Maybe MisoString,
    mStatus :: GameStatus,
    mMaxGuesses :: Int,
    mDarkMode :: Bool,
    mShowRooms :: Bool,
    mRoomFilter :: MisoString
  }
  deriving (Eq)

data Action
  = UpdateInput MisoString
  | SelectSuggestion MisoString
  | SubmitGuess
  | GiveUp
  | NewGame
  | SetAnswer Room
  | ToggleTheme
  | ToggleRooms
  | UpdateRoomFilter MisoString
  | PickRoomFromModal MisoString
  deriving (Eq)

main :: IO ()
main = do
  initialAns <- pickRandomRoom
  startApp defaultEvents (mkApp initialAns)

pickRandomRoom :: IO Room
pickRandomRoom = do
  idx <- randomRIO (0, length allRooms - 1)
  pure (allRooms !! idx)

mkApp :: Room -> App Model Action
mkApp initialAns = component (initialModel initialAns) updateModel viewModel

initialModel :: Room -> Model
initialModel ans =
  Model
    { mAnswer = ans,
      mGuesses = [],
      mInput = "",
      mError = Nothing,
      mStatus = Playing,
      mMaxGuesses = defaultMaxGuesses,
      mDarkMode = False,
      mShowRooms = False,
      mRoomFilter = ""
    }

updateModel :: Action -> Effect parent props Model Action
updateModel = \case
  UpdateInput txt ->
    modify $ \m -> m {mInput = txt, mError = Nothing}
  SelectSuggestion roomName -> do
    modify $ \m -> m {mInput = roomName, mError = Nothing}
    updateModel SubmitGuess
  SubmitGuess -> do
    m <- get
    case mStatus m of
      Playing ->
        let raw = fromMisoString (mInput m)
            trimmed = trim raw
         in case trimmed of
              "" -> pure ()
              _ -> case findRoom trimmed of
                Nothing ->
                  modify $ \s -> s {mError = Just (ms ("Unknown room: " ++ trimmed))}
                Just guess ->
                  let res = guessRoom (mAnswer m) guess
                      newGuesses = (guess, res) : mGuesses m
                      newStatus
                        | guess == mAnswer m = Won
                        | length newGuesses >= mMaxGuesses m = Lost
                        | otherwise = Playing
                   in put
                        m
                          { mGuesses = newGuesses,
                            mInput = "",
                            mError = Nothing,
                            mStatus = newStatus
                          }
      _ -> pure ()
  GiveUp ->
    modify $ \m ->
      if mStatus m == Playing
        then m {mStatus = Lost, mError = Nothing}
        else m
  NewGame ->
    io (SetAnswer <$> pickRandomRoom)
  SetAnswer ans ->
    modify $ \m ->
      (initialModel ans)
        { mDarkMode = mDarkMode m
        }
  ToggleTheme ->
    modify $ \m -> m {mDarkMode = not (mDarkMode m)}
  ToggleRooms ->
    modify $ \m -> m {mShowRooms = not (mShowRooms m), mRoomFilter = ""}
  UpdateRoomFilter txt ->
    modify $ \m -> m {mRoomFilter = txt}
  PickRoomFromModal roomName ->
    modify $ \m ->
      m
        { mShowRooms = False,
          mRoomFilter = "",
          mInput = if mStatus m == Playing then roomName else mInput m,
          mError = Nothing
        }

-- | String helpers matching CLI normalization.
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

suggestRooms :: String -> [Room]
suggestRooms input =
  let key = normalize input
   in if null key
        then []
        else take 8 [r | r <- allRooms, key `isInfixOf` normalize (name r)]

-- | View
viewModel :: Model -> View c p Model Action
viewModel m =
  H.div_
    [ P.class_ "app-shell mie",
      textProp "data-theme" (if mDarkMode m then "dark" else "light")
    ]
    [ viewHeaderBar m,
      H.main_
        [P.class_ "app-clamp"]
        [ viewStatusBanner m,
          viewGuessInput m,
          viewGuessesCard m,
          viewLegend
        ],
      if mShowRooms m then viewRoomsModal m else text ""
    ]

viewHeaderBar :: Model -> View c p Model Action
viewHeaderBar m =
  H.header_
    [P.class_ "mie headerbar app-headerbar"]
    [ H.div_
        [P.class_ "left"]
        [ H.button_
            [ P.type_ "button",
              P.class_ "mie button",
              E.onClick ToggleRooms
            ]
            [text "Rooms"]
        ],
      H.div_
        [P.class_ "center"]
        [ H.div_
            [P.class_ "header-title-wrap"]
            [ H.span_ [P.class_ "header-title"] [text "Blue Prince Room Guesser"],
              H.span_
                [P.class_ "header-subtitle"]
                [ text $
                    ms $
                      "Guess "
                        ++ show (length (mGuesses m))
                        ++ " of "
                        ++ show (mMaxGuesses m)
                ]
            ]
        ],
      H.div_
        [P.class_ "right"]
        [ H.button_
            [ P.type_ "button",
              P.class_ "mie button",
              P.title_ "Toggle light/dark theme",
              E.onClick ToggleTheme
            ]
            [text (if mDarkMode m then "☀" else "🌙")],
          H.button_
            [ P.type_ "button",
              P.class_ "mie button",
              E.onClick NewGame
            ]
            [text "New Game"],
          if mStatus m == Playing
            then
              H.button_
                [ P.type_ "button",
                  P.class_ "mie button error",
                  E.onClick GiveUp
                ]
                [text "Give Up"]
            else text ""
        ]
    ]

viewStatusBanner :: Model -> View c p Model Action
viewStatusBanner m = case mStatus m of
  Won ->
    H.div_
      [P.class_ "status-banner win"]
      [ H.span_
          []
          [ text $
              ms $
                "Correct! You guessed "
                  ++ name (mAnswer m)
                  ++ " in "
                  ++ show (length (mGuesses m))
                  ++ "/"
                  ++ show (mMaxGuesses m)
                  ++ " guesses."
          ],
        H.button_
          [ P.type_ "button",
            P.class_ "mie button filled accent",
            E.onClick NewGame
          ]
          [text "Play Again"]
      ]
  Lost ->
    H.div_
      [P.class_ "status-banner lose"]
      [ H.span_
          []
          [text $ ms $ "Out of guesses — the room was: " ++ name (mAnswer m)],
        H.button_
          [ P.type_ "button",
            P.class_ "mie button filled accent",
            E.onClick NewGame
          ]
          [text "Play Again"]
      ]
  Playing -> case mError m of
    Nothing -> text ""
    Just err ->
      H.div_
        [P.class_ "status-banner warn"]
        [H.span_ [] [text err]]

viewGuessInput :: Model -> View c p Model Action
viewGuessInput m =
  let raw = fromMisoString (mInput m)
      suggestions = if mStatus m == Playing then suggestRooms raw else []
      exactMatch = findRoom raw
      showPopover = not (null suggestions) && exactMatch == Nothing
      disabledAttr = [P.disabled_ | mStatus m /= Playing]
   in H.section_
        [P.class_ "guess-input-section"]
        [ H.form_
            [ P.class_ "guess-input-row",
              E.onSubmit SubmitGuess
            ]
            [ H.div_
                [P.class_ "guess-input-field"]
                [ H.input_
                    ( [ P.type_ "text",
                        P.placeholder_ "Type a room name…",
                        P.value_ (mInput m),
                        E.onInput UpdateInput
                      ]
                        ++ disabledAttr
                    )
                ],
              H.button_
                ( [ P.type_ "submit",
                    P.class_ "mie button filled accent"
                  ]
                    ++ disabledAttr
                )
                [text "Guess"]
            ],
          if showPopover
            then
              H.div_
                [P.class_ "autocomplete-popover"]
                (map viewSuggestionItem suggestions)
            else text ""
        ]

viewSuggestionItem :: Room -> View c p Model Action
viewSuggestionItem r =
  H.button_
    [ P.type_ "button",
      P.class_ "autocomplete-item",
      E.onClick (SelectSuggestion (ms (name r)))
    ]
    [ H.span_ [] [text (ms (name r))],
      H.span_
        [P.class_ "autocomplete-meta"]
        [text (ms (show (shape r) ++ " · " ++ show (gemCost r) ++ "💎"))]
    ]

viewGuessesCard :: Model -> View c p Model Action
viewGuessesCard m =
  H.section_
    [P.class_ "guesses-card"]
    [ if null (mGuesses m)
        then
          H.div_
            [P.class_ "empty-state"]
            [text "Enter a room name above to make your first guess. Each guess appears as a row so you can scan constraints down each column."]
        else
          H.div_
            [P.class_ "guesses-table-scroll"]
            [ H.table_
                [P.class_ "guesses-table"]
                [ viewTableHeader,
                  H.tbody_ [] (map (uncurry viewGuessRow) (mGuesses m))
                ]
            ]
    ]

viewTableHeader :: View c p Model Action
viewTableHeader =
  H.thead_
    []
    [ H.tr_
        []
        [ H.th_ [] [text "Room"],
          H.th_ [] [text "Shape"],
          H.th_ [] [text "Colors"],
          H.th_ [] [text "Cost"],
          viewHelpHeader
            "Pool"
            "Pool options"
            ["Starting", "Found", "Outer", "Studio", "Upgradable"],
          viewHelpHeader
            "Special"
            "Special categories"
            ["None", "Drafting", "Entry", "Mechanical", "Permanent", "Puzzle", "Spread", "Tomorrow"],
          H.th_ [P.class_ "emoji-col-header", P.title_ "Steps"] [text "👣"],
          H.th_ [P.class_ "emoji-col-header", P.title_ "Coins"] [text "🪙"],
          H.th_ [P.class_ "emoji-col-header", P.title_ "Gems"] [text "💎"],
          H.th_ [P.class_ "emoji-col-header", P.title_ "Keys"] [text "🔑"],
          H.th_ [P.title_ "Other resources / items"] [text "Other"]
        ]
    ]

viewHelpHeader :: MisoString -> MisoString -> [MisoString] -> View c p Model Action
viewHelpHeader label title options =
  H.th_
    [P.class_ "help-th", P.tabindex_ "0"]
    [ H.span_
        [P.class_ "help-label"]
        [ text label,
          H.span_ [P.class_ "help-badge"] [text "?"]
        ],
      H.div_
        [P.class_ "help-popover"]
        [ H.span_ [P.class_ "help-popover-title"] [text title],
          text (ms (intercalate ", " (map fromMisoString options)))
        ]
    ]

viewGuessRow :: Room -> RoomGuessWithResult -> View c p Model Action
viewGuessRow guess res =
  let (shp, shpRes) = shapeWithResult res
      (cols, colsRes) = colorsWithResult res
      (gems, gemsRes, gemsOrd) = gemCostWithResult res
      (pool, poolRes) = poolCatWithResult res
      (specs, specsRes) = specialCatsWithResult res
      resCell r = case Map.lookup r (resourcesWithResult res) of
        Nothing -> H.td_ [] [text "-"]
        Just (gain, rRes, pOrd) ->
          let arrow = case getCmp pOrd of
                Just LT -> " ↓"
                Just GT -> " ↑"
                _ -> ""
           in H.td_ [] [viewBadge rRes (show gain ++ arrow)]
   in H.tr_
        []
        [ H.td_ [] [text (ms (name guess))],
          H.td_ [] [viewBadge shpRes (show shp)],
          H.td_ [] [viewBadge colsRes (formatSet cols)],
          H.td_ [] [viewBadge gemsRes (show gems ++ formatOrd gemsOrd)],
          H.td_ [] [viewBadge poolRes (show pool)],
          H.td_ [] [viewBadge specsRes (formatSet specs)],
          resCell Steps,
          resCell Coins,
          resCell Gems,
          resCell Keys,
          resCell Other
        ]

viewBadge :: Result -> String -> View c p Model Action
viewBadge res label =
  let cls = case res of
        Correct -> "cell-badge correct"
        Partial -> "cell-badge partial"
        Wrong -> "cell-badge wrong"
   in H.span_ [P.class_ cls] [text (ms label)]

formatSet :: (Show a) => Set a -> String
formatSet s
  | Set.null s = "None"
  | otherwise = intercalate ", " (map show (Set.toList s))

formatOrd :: Ordering -> String
formatOrd = \case
  LT -> " ↓"
  GT -> " ↑"
  EQ -> ""

viewLegend :: View c p Model Action
viewLegend =
  H.div_
    [P.class_ "legend-row"]
    [ H.span_ [P.class_ "legend-item"] [viewBadge Correct "Blue", text "Exact match"],
      H.span_ [P.class_ "legend-item"] [viewBadge Partial "Yellow", text "Partial / same-sign"],
      H.span_ [P.class_ "legend-item"] [viewBadge Wrong "Red", text "Wrong"],
      H.span_ [P.class_ "legend-item"] [text "↑ / ↓ Target is higher / lower"]
    ]

viewRoomsModal :: Model -> View c p Model Action
viewRoomsModal m =
  let query = normalize (fromMisoString (mRoomFilter m))
      filtered =
        [ r
          | r <- allRooms,
            null query || query `isInfixOf` normalize (name r)
        ]
   in H.div_
        [P.class_ "modal-backdrop"]
        [ H.div_
            [ P.class_ "modal-backdrop-bg",
              E.onClick ToggleRooms
            ]
            [],
          H.div_
            [P.class_ "rooms-dialog"]
            [ H.div_
                [P.class_ "rooms-dialog-header"]
                [ H.span_ [] [text (ms ("All Rooms (" ++ show (length allRooms) ++ ")"))],
                  H.button_
                    [ P.type_ "button",
                      P.class_ "mie button",
                      E.onClick ToggleRooms
                    ]
                    [text "Close"]
                ],
              H.div_
                [P.class_ "rooms-dialog-body"]
                [ H.div_
                    [P.class_ "guess-input-field"]
                    [ H.input_
                        [ P.type_ "text",
                          P.placeholder_ "Filter rooms…",
                          P.value_ (mRoomFilter m),
                          E.onInput UpdateRoomFilter
                        ]
                    ],
                  H.div_
                    [P.class_ "rooms-list"]
                    (map viewRoomChip filtered)
                ]
            ]
        ]

viewRoomChip :: Room -> View c p Model Action
viewRoomChip r =
  H.button_
    [ P.type_ "button",
      P.class_ "room-chip",
      E.onClick (PickRoomFromModal (ms (name r)))
    ]
    [ H.span_ [] [text (ms (name r))],
      H.span_
        [P.class_ "room-chip-sub"]
        [text (ms (show (shape r) ++ " · " ++ show (poolCat r) ++ " · " ++ show (gemCost r) ++ "💎"))]
    ]
