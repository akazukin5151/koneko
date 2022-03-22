module History where

import Data.List ( sortBy, partition )
import Lens.Micro ( (&), (<&>) )
import Types ( Mode, St )
import qualified Data.Text as T
import Data.Text (Text)
import Core (highlightedMode, modeIdxtoMode)
import Core (strToInt)

parseHistoryFile :: St -> Text -> [(Text, Int)]
parseHistoryFile st file =
  file
  & T.words
  <&> parseHistoryLine
  & filter (\(mode, _) -> mode == highlightedMode st)
  <&> snd
  & count
  & sortByRev (\(_, a) (_, b) -> compare a b)

type Input = Text

parseHistoryLine :: Text -> (Mode, Input)
parseHistoryLine line = (mode, freq)
  where
    mode =
      line
      & T.take 1
      & T.unpack
      & strToInt
      & modeIdxtoMode
    freq = T.drop 2 line

sortByRev :: (b -> b -> Ordering) -> [b] -> [b]
sortByRev f = sortBy (flip f)

-- | Counts the number of occurrences of every item in a list,
-- returning a list of each unique item and their frequency
--
-- >>> count [1, 2, 3, 1, 2, 1]
-- [(1, 3), (2, 2), (3, 1)]
-- >>> count ['a', 'b', 'a', 'c', 'b']
-- [('b', 2), ('c', 1), ('a', 2)]
count :: Eq a => [a] -> [(a, Int)]
count = go []
  where
    go :: Eq a => [(a, Int)] -> [a] -> [(a, Int)]
    go res [] = res
    go res (x':xs) =
      if x' `elem` (fst <$> res)
         then do
           -- clean_res is res without the tuple storing (freq, x')
           -- there will always be only one item in res where s == x'
           -- because res is unique
           let ([old_tup], clean_res) = partition (\(s, _) -> s == x') res
           go ((x', snd old_tup + 1) : clean_res) xs
         else go ((x', 1) : res) xs
