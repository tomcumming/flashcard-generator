module Tatoeba.Frequency (frequency) where

import Data.Aeson.Text (encodeToLazyText)
import Data.Conduit
import qualified Data.Conduit.Combinators as C
import Data.Foldable (fold)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Lazy (toStrict)
import SentGen.Tokenise.WhiteSpace (tokenise)

parseInputLine :: T.Text -> Maybe T.Text
parseInputLine line = case T.splitOn "\t" line of
  [_s1, s2] -> Just s2
  _ -> Nothing

parseInputLines :: ConduitT T.Text T.Text IO ()
parseInputLines =
  C.mapM $
    maybe
      (error "Failed to parse input file")
      pure
      . parseInputLine

type PhraseFreq = Map.Map (Seq.Seq T.Text) Word

orderPhrases :: PhraseFreq -> Seq.Seq (Word, Seq.Seq T.Text)
orderPhrases =
  Seq.sortOn ((maxBound -) . fst)
    . Seq.fromList
    . fmap (\(f, s) -> (s, f))
    . filter (\(phr, c) -> Seq.length phr == 1 || c > 1)
    . Map.toAscList

writePhraseFreq :: Seq.Seq (Word, Seq.Seq T.Text) -> IO ()
writePhraseFreq =
  mapM_
    ( \(c, phr) ->
        T.putStrLn $
          fold
            [ T.pack (show c),
              "\t",
              toStrict $ encodeToLazyText phr
            ]
    )

countPhraseFreq :: Seq.Seq T.Text -> PhraseFreq
countPhraseFreq = \case
  Seq.Empty -> mempty
  toks ->
    Map.unionWith
      (+)
      (Map.fromList $ map (\n -> (Seq.take n toks, 1)) [1 .. 4])
      $ countPhraseFreq (Seq.drop 1 toks)

-- Because i can't get foldl to work
sumPhraseFreqs :: ConduitT PhraseFreq PhraseFreq IO ()
sumPhraseFreqs = go mempty
  where
    go prev = do
      maybeNext <- await
      case maybeNext of
        Nothing -> yield prev
        Just next -> go $ Map.unionWith (+) prev next

frequency :: ConduitT T.Text Void IO ()
frequency =
  parseInputLines
    .| C.map tokenise
    .| C.map countPhraseFreq
    .| sumPhraseFreqs
    .| C.map orderPhrases
    .| C.mapM_ writePhraseFreq
