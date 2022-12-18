module Tatoeba.Convert (convert) where

import Control.Monad.Trans (lift)
import Data.Char (isLetter)
import Data.Conduit
import Data.Conduit.Combinators qualified as C
import Data.Foldable (fold)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.IO (stderr)

naiveValidSentence :: T.Text -> Bool
naiveValidSentence = T.any isLetter

parseTsvLine :: T.Text -> Maybe (T.Text, T.Text)
parseTsvLine line = case T.splitOn "\t" line of
  [_id1, s1, _id2, s2]
    | naiveValidSentence s1
        && naiveValidSentence s2 ->
        Just (s1, s2)
  _ -> Nothing

writePairTsvLine :: T.Text -> T.Text -> IO ()
writePairTsvLine s1 s2 = T.putStrLn $ fold [s1, "\t", s2]

parseTsvLines :: ConduitT T.Text (T.Text, T.Text) IO ()
parseTsvLines = go 0
  where
    go :: Word -> ConduitT T.Text (T.Text, T.Text) IO ()
    go failures = do
      maybeLine <- await
      case maybeLine of
        Nothing
          | failures > 0 ->
              lift $
                T.hPutStrLn stderr $
                  "Failed to parse " <> T.pack (show failures) <> " line(s)"
        Nothing -> pure ()
        Just line
          | Just pair <- parseTsvLine line -> yield pair >> go failures
          | otherwise -> go (succ failures)

convert :: ConduitT T.Text Void IO ()
convert =
  parseTsvLines
    .| C.mapM_ (uncurry writePairTsvLine)
