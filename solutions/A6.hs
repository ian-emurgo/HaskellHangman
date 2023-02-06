{-# LANGUAGE InstanceSigs #-}

module A6 where

import Provided

import Data.Char ( isAlpha, toUpper, toLower )
import Data.List ( intersperse, sort )

-- *** A6-0: WARM-UP *** --

-- Q#01
type Chances    = Int
type Guess      = String
type Move       = Char
type Secret     = String
type Dictionary = [String]

-- Q#02
data GameException =  InvalidChars | InvalidLength | NotInDict | InvalidMove | RepeatMove | GameOver

-- Q#03
lengthInRange :: Secret -> Bool
lengthInRange s = length s >= fst _LENGTH_ && length s <= snd _LENGTH_

-- Q#04
invalidMove :: Move -> Bool
invalidMove move = not (isAlpha move)

-- Q#05
revealLetters :: Move -> Secret -> Guess -> Guess
revealLetters move secret guess = zipWith (\s g -> if s == move then s else g) secret guess

-- Q#06
updateChances :: Move -> Secret -> Chances -> Chances
updateChances move secret chances = if move `elem` secret then chances else chances - 1

-- Q#07
setSecret :: IO Secret
setSecret = do
  putStr "Enter a secret word:\t"
  showInput False
  secret <- getLine
  showInput True
  _SPACE_
  return secret

-- *** A6-1: Records & Instances *** --

-- Q#08
data Game =
  Game { getSecret  :: Secret
       , getGuess   :: Guess
       , getMoves   :: [Move]
       , getChances :: Int
       }

-- Q#09
repeatedMove :: Move -> Game -> Bool
repeatedMove move game = move `elem` getMoves game

-- Q#10
makeGame :: Secret -> Game
makeGame secret =
  Game { getSecret  = map toUpper secret
       , getGuess   = map (const '_') secret
       , getMoves   = []
       , getChances = _CHANCES_
       }

-- Q#11
updateGame :: Move -> Game -> Game
updateGame move game =
  game { getGuess = newGuess
       , getMoves = move : getMoves game
       , getChances = newChances
       }
  where
    newGuess   = revealLetters move (getSecret game) (getGuess game)
    newChances = updateChances move (getSecret game) (getChances game)

-- Q#12
showGameHelper :: Guess -> [Move] -> Chances -> String
showGameHelper g ms cs = unlines [
      _STARS_
    , "\tSecret Word:\t" ++ intersperse ' ' g ++ "\n"
    , "\tGuessed:\t" ++ intersperse ' ' (sort ms) ++ "\n"
    , "\tChances:\t" ++ show cs
    , _STARS_
    ]

instance Show Game where
  show :: Game -> String
  show (Game _ g ms cs) = showGameHelper g ms cs

-- Q#13
instance Show GameException where
  show :: GameException -> String
  show InvalidChars  = "Word contains invalid characters: try again!"
  show InvalidLength = concat ["Word must be between ", lb, " and ", ub, " characters: try again!"]
    where
      lb = show $ fst _LENGTH_
      ub = show $ snd _LENGTH_
  show NotInDict   = "Word is not in dictionary: try again!"
  show InvalidMove = "Invalid character: try again!"
  show RepeatMove  = "You guessed that already: try again!"
  show GameOver    = "Sorry, game over!"


-- *** A6-2: Exception Contexts *** --

-- Q#14
toMaybe :: Bool -> a -> Maybe a
toMaybe b a = if b then Just a else Nothing

-- Q#15
validateSecret :: (Secret -> Bool) -> GameException -> Secret -> Either GameException Secret
validateSecret p e s = if p s then Right s else Left e

-- Q#16
hasValidChars :: Secret -> Either GameException Secret
hasValidChars = validateSecret (all isAlpha) InvalidChars

isValidLength :: Secret -> Either GameException Secret
isValidLength = validateSecret lengthInRange InvalidLength

isInDict :: Dictionary -> Secret -> Either GameException Secret
isInDict dict = validateSecret (\s -> map toLower s `elem` dict) NotInDict

-- Q#17
validateNoDict :: Secret -> Either GameException Secret
validateNoDict s = case hasValidChars s of
  Right s' -> isValidLength s'
  err -> err

validateWithDict :: Dictionary -> Secret -> Either GameException Secret
validateWithDict dict s = case validateNoDict s of
  Right s' -> isInDict dict s'
  err -> err

-- Q#18
processTurn :: Move -> Game -> Either GameException Game
processTurn move game
  | invalidMove move        = Left InvalidMove
  | repeatedMove move game  = Left RepeatMove
  | getChances newGame == 0 = Left GameOver
  | otherwise               = Right newGame
  where
    newGame = updateGame move game