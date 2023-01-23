{-# LANGUAGE InstanceSigs #-}

module Sandbox where

data Name = Name String String


data Maybe' a = Nothing' | Just' a
  deriving Show

data Either' e a = Left' e | Right' a
  deriving Show

instance Functor Maybe' where
  fmap :: (a -> b) -> Maybe' a -> Maybe' b
  fmap _   Nothing' = Nothing'
  fmap ab (Just' a) = Just' (ab a)

-- fmap _ []     = []
-- fmap f (x:xs) = f x : fmap f xs

instance Applicative Maybe' where
  pure :: a -> Maybe' a
  pure a = Just' a

  (<*>) :: Maybe' (a -> b) -> Maybe' a -> Maybe' b
  -- (+) <$> Nothing <*> Just 2
  -- (+) :: Num a => a -> (a -> a)
  -- (+) <$> Nothing :: Maybe (a -> a) == Nothing
  Nothing' <*> _   = Nothing'
  Just' ab <*> ma  = fmap ab ma

instance Functor (Either' e) where
  fmap :: (a -> b) -> Either' e a -> Either' e b
  fmap _  (Left'  e) = Left' e
  fmap ab (Right' a) = Right' $ ab a

instance Applicative (Either' e) where
  pure :: a -> Either' e a
  pure a = Right' a

  (<*>) :: Either' e (a -> b) -> Either' e a -> Either' e b
  Left' e   <*> _  = Left' e
  Right' ab <*> ea = ab <$> ea