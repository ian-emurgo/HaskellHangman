module A7 where

import Provided
import A6

import Data.Char ( toUpper )
import System.Directory ( doesFileExist )

-- *** A7: Functors & Applicatives *** --

-- Q#01
getUpperChar :: IO Char
getUpperChar = toUpper <$> getChar

-- Q#02
_DICT_ :: IO Dictionary
_DICT_ = words <$> readFile _DICT_FILE_

-- Q#03
makeGameIfValid :: Either GameException Secret -> Either GameException Game
makeGameIfValid secret = makeGame <$> secret

-- Q#04
getDict :: IO (Maybe Dictionary)
getDict = toMaybe <$> doesFileExist _DICT_FILE_ <*> _DICT_