module Main where

import Control.Monad.Trans (lift)
import Data.Char (isLetter)
import Data.Conduit
import qualified Data.Conduit.Combinators as C
import Data.Foldable (fold)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative
import System.IO (stderr)

data Options
  = Convert

parseOptions :: Parser Options
parseOptions =
  subparser
    (command "convert" (info (pure Convert) (progDesc "Convert Tatoeba language pairs export file")))

parseInfo :: ParserInfo Options
parseInfo =
  info (parseOptions <**> helper) $
    fullDesc <> progDesc "Toolkit for Tatoeba data"

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
              T.hPutStrLn stderr $ "Failed to parse " <> T.pack (show failures) <> " line(s)"
        Nothing -> pure ()
        Just line
          | Just pair <- parseTsvLine line -> yield pair >> go failures
          | otherwise -> go (succ failures)

stdinLines :: ConduitT a T.Text IO ()
stdinLines =
  C.stdin
    .| C.decodeUtf8
    .| C.linesUnbounded

main :: IO ()
main = do
  opts <- execParser parseInfo
  case opts of
    Convert ->
      runConduit $
        stdinLines
          .| parseTsvLines
          .| C.mapM_ (uncurry writePairTsvLine)
