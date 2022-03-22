module Core where

import Data.Maybe (fromJust)
import Lens.Micro ( (^.) )
import Types
    ( modeIdx,
      Mode(SingleIllustration, FollowingArtists, SearchArtists,
           FollowingArtistsIllustrations, RecommendedIllustrations, Info,
           Manual, BrowseCache, ArtistIllustrations),
      St, )

modes :: [Mode]
modes =
  [ ArtistIllustrations
  , SingleIllustration
  , FollowingArtists
  , SearchArtists
  , FollowingArtistsIllustrations
  , RecommendedIllustrations
  , Info
  , Manual
  , BrowseCache
  ]

enumerate :: [a] -> [(Int, a)]
enumerate xs = zip [0 .. length xs] xs

modeIdxtoMode :: Int -> Mode
modeIdxtoMode idx = fromJust $ lookup idx $ enumerate modes

highlightedMode :: St -> Mode
highlightedMode st = modeIdxtoMode $ st^.modeIdx

-- | Just @show@ but with restricted polymorphism to prevent misuse
--
-- This is normal and correct behaviour
--
-- >>> test :: String
-- >>> test = show 2
--
-- This is a bug where a String was passed instead, but it compiles!
-- The purpose is to convert non-strings into strings, so there is no need
-- to @show@ a string.
--
-- >>> test2 :: String
-- >>> test2 = show "no"
intToStr :: Int -> String
intToStr = show

-- | Just @read@ but with a better name and type in parallel to intToStr
strToInt :: String -> Int
strToInt = read
