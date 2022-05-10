module History where

import Data.List ( sortBy, partition )
import Lens.Micro ( (&), (<&>) )
import Types ( Mode, St )
import qualified Data.Text as T
import Data.Text (Text)
import Core (highlightedMode, modeIdxtoMode)
import Core (strToInt)

type Input = Text

parseHistoryFile :: Text -> [(Mode, Input)]
parseHistoryFile file =
  file
  & T.words
  <&> parseHistoryLine
  & countBy snd
  & sortByRev (\(_, a) (_, b) -> compare a b)
  <&> fst

parseHistoryLine :: Text -> (Mode, Input)
parseHistoryLine line = (mode, input)
  where
    mode =
      line
      & T.take 1
      & T.unpack
      & strToInt
      & modeIdxtoMode
    input = T.drop 2 line

sortByRev :: (b -> b -> Ordering) -> [b] -> [b]
sortByRev f = sortBy (flip f)

-- | Counts the number of occurrences of every item in a list,
-- returning a list of each unique item and their frequency
--
-- >>> countBy [1, 2, 3, 1, 2, 1]
-- [(1, 3), (2, 2), (3, 1)]
-- >>> countBy ['a', 'b', 'a', 'c', 'b']
-- [('b', 2), ('c', 1), ('a', 2)]
countBy :: (Eq a, Eq b) => (a -> b) -> [a] -> [(a, Int)]
countBy f = go f []
  where
    go :: (Eq a, Eq b) => ((a -> b) -> [(a, Int)] -> [a] -> [(a, Int)])
    go _ res [] = res
    go g res (x':xs) = do
      if g x' `elem` (g . fst <$> res)
         then do
           -- clean_res is res without the tuple storing (freq, x')
           -- there will always be only one item in res where s == x'
           -- because res is unique
           let ([old_tup], clean_res) = partition (\(s, _) -> s == x') res
           go g ((x', snd old_tup + 1) : clean_res) xs
         else go g ((x', 1) : res) xs
