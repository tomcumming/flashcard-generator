module MyLib (someFunc) where

import Data.Char (isLetter)
import Data.Sequence (Seq, fromList)
import qualified Data.Text as T

tokenize :: T.Text -> Seq T.Text
tokenize =
  fromList
    . fmap T.toLower
    . filter (T.any isLetter)
    . T.split (not . isLetter)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
