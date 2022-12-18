module Main where

import Data.Conduit
import qualified Data.Conduit.Combinators as C
import Data.Foldable (fold)
import qualified Data.Text as T
import Options.Applicative
import Tatoeba.Convert (convert)
import Tatoeba.Frequency (frequency)

data Options
  = Convert
  | Frequency

parseOptions :: Parser Options
parseOptions =
  subparser $
    fold
      [ command
          "convert"
          (info (pure Convert) (progDesc "Convert Tatoeba language pairs export file")),
        command
          "frequency"
          (info (pure Frequency) (progDesc "Calculate phrase frequency information"))
      ]

parseInfo :: ParserInfo Options
parseInfo =
  info (parseOptions <**> helper) $
    fullDesc <> progDesc "Toolkit for Tatoeba data"

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
          .| convert
    Frequency ->
      runConduit $
        stdinLines
          .| frequency
