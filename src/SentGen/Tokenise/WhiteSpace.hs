module SentGen.Tokenise.WhiteSpace (tokenise) where

import Data.Char (isLetter)
import Data.Sequence (Seq, fromList)
import qualified Data.Text as T

tokenise :: T.Text -> Seq T.Text
tokenise =
  fromList
    . fmap T.toLower
    . filter (T.any isLetter)
    . T.split (not . isLetter)
