module A8 where

import Provided
import A6 hiding ( validateNoDict, validateWithDict )
import A7

import Control.Monad
import Control.Monad.State
import Data.Char ( toUpper )

-- *** A8: Monads *** --

-- Q#01
validateNoDict :: Secret -> Either GameException Secret
validateNoDict s = hasValidChars s >>= isValidLength

validateWithDict :: Dictionary -> Secret -> Either GameException Secret
validateWithDict dict s = validateNoDict s >>= isInDict dict

-- Q#02
playGame :: Game -> IO ()
playGame game = do
  promptGuess
  move <- getUpperChar
  _SPACE_
  case processTurn move game of
    Left GameOver -> print GameOver >> putStrLn ("The word was: " ++ getSecret game)
    Left e        -> print e >> playGame game
    Right game'   -> do
      print game'
      if   getGuess game' == getSecret game'
      then putStrLn "Yay, Correct!"
      else playGame game'

-- Q#03
startGame :: (Secret -> Either GameException Secret) -> IO ()
startGame validator = do
  secret <- setSecret
  case makeGameIfValid (validator secret) of
    Left  e -> print e >> startGame validator
    Right g -> print g >> playGame g

-- Q#04
runApp :: IO ()
runApp = do
  maybeDict <- getDict
  case maybeDict of
    Just dict -> startGame (validateWithDict dict)
    Nothing   -> do
      putStrLn "Missing dictionary file! Continue without dictionary? [Y/N]"
      c <- getUpperChar
      when (c == 'Y') $ startGame validateNoDict -- exit if not 'Y'


-- Q#05
makeGameS :: Secret -> State Game ()
makeGameS secret = put $
  Game { getSecret  = map toUpper secret
       , getGuess   = map (const '_') secret
       , getMoves   = []
       , getChances = _CHANCES_
       }

updateGameS :: Move -> State Game ()
updateGameS move = modify (updateGame move)

repeatedMoveS :: Move -> State Game Bool
repeatedMoveS move = do
  moves <- gets getMoves
  pure $ move `elem` moves

processTurnS :: Move -> State Game (Either GameException ())
processTurnS move | invalidMove move = pure $ Left InvalidMove
processTurnS move = do
  r <- repeatedMoveS move
  if r then pure $ Left RepeatMove else do
    updateGameS move
    chances <- gets getChances
    if chances == 0 then pure $ Left GameOver else pure $ Right ()
