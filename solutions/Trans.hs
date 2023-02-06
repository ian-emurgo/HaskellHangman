-- This pragma is required to use concrete types (like `Game` and `GameException`) in our typeclass
-- constraints, which is required to use the `MonadState` and `MonadError` mtl typeclasses.
{-# LANGUAGE FlexibleContexts #-}

module Trans ( runAppT ) where

import Provided
import A6
import A7
import A8

import Control.Monad ( when, void )
import Control.Monad.Except ( MonadError, throwError, tryError, catchError, runExceptT )
import Control.Monad.State  ( MonadIO, MonadState, get, gets, modify, put, evalStateT, liftIO )
import Data.Char ( isAlpha, toLower )
import System.IO

runAppT :: IO () -- refactored `main` action
runAppT = do
  maybeDict <- getDict
  case maybeDict of
    Just dict -> void . runExceptT . startGameT $ validateWithDictT dict
    -- Use `void` to discard the final `Either GameException ()` value, replacing it with `()`
      -- We handle all possible game exceptions elsewhere, so we should always get `Right ()` here
    Nothing   -> do
      putStrLn "Missing dictionary file! Continue without dictionary? [Y/N]"
      c <- getUpperChar
      when (c == 'Y') $ void . runExceptT . startGameT $ validateNoDictT

-- This action will work with any transformer containing IO and exception-handling functionality:
startGameT :: ( MonadIO m
              , MonadError GameException m) => (Secret -> m Secret) -> m ()

startGameT validator = do
  secret  <- liftIO setSecret -- `liftIO` is required to run IO actions inside the transformer
  vSecret <- tryError $ validator secret -- `tryError` is a utility of the `MonadError` class
    -- it returns an `Either GameException Secret` value inside our transformer context
  either handler (play . makeGame) vSecret -- if invalid secret, call the handler to restart
                                           -- otherwise, make initial game state and play
  where
    handler err = do -- just print the error and start again
      liftIO $ print err
      startGameT validator
    play :: (MonadIO m, MonadError GameException m) => Game -> m ()
    play game = do -- print initial state, then run the stateful `playGame` loop with it
      liftIO $ print game
      evalStateT playGameT game

-- This action will work with any transformer containing IO, exception-handling, and state-handling
  -- functionality:
playGameT :: (MonadIO m, MonadError GameException m, MonadState Game m) => m ()
playGameT = do
  liftIO promptGuess
  move <- liftIO getUpperChar
  liftIO _SPACE_
  catchError (loop move) handler -- if `processTurnT` throws error in loop, pass control to handler
  where
    loop :: (MonadIO m, MonadError GameException m, MonadState Game m) => Move -> m ()
    loop move = do
      processTurnT move
      game <- get
      liftIO $ print game
      if getGuess game == getSecret game
        then liftIO $ putStrLn "Yay, Correct!" -- break loop
        else playGameT -- continue
    handler :: (MonadIO m, MonadError GameException m, MonadState Game m) => GameException -> m ()
    handler GameOver = do
      liftIO $ print GameOver
      secret <- gets getSecret
      liftIO $ putStrLn ("The word was: " ++ secret)
    handler e        = do
      liftIO $ print e
      playGameT

processTurnT :: (MonadState Game m, MonadError GameException m) => Move -> m ()
processTurnT move = do
  when   (invalidMove move)       (throwError InvalidMove)
  -- `throwError` is a utility of the `MonadError` class: it raises an exception, short-circuiting
    -- any further computation. Exceptions thrown in `processTurnT` will be caught and handled by
    -- `playGameT`.
  game    <- get
  when   (repeatedMove move game) (throwError RepeatMove)
  modify (updateGame move)
  chances <- gets getChances
  when   (chances == 0)           (throwError GameOver)

-- We also need to revise our validators to have more polymorphic signatures, so they're compatible
-- with our refactored actions.
validateSecretT :: MonadError GameException m =>
                  (Secret -> Bool) -> GameException -> Secret -> m Secret
validateSecretT p e s = if p s then pure s else throwError e

hasValidCharsT :: MonadError GameException m => Secret -> m Secret
hasValidCharsT = validateSecretT (all isAlpha) InvalidChars

isValidLengthT :: MonadError GameException m => Secret -> m Secret
isValidLengthT = validateSecretT lengthInRange InvalidLength

isInDictT :: MonadError GameException m => Dictionary -> Secret -> m Secret
isInDictT dict = validateSecretT (\s -> map toLower s `elem` dict) NotInDict

validateNoDictT :: MonadError GameException m => Secret -> m Secret
validateNoDictT s = hasValidCharsT s >>= isValidLengthT

validateWithDictT :: MonadError GameException m => Dictionary -> Secret -> m Secret
validateWithDictT dict s = validateNoDictT s >>= isInDictT dict