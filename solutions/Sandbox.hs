{-# LANGUAGE InstanceSigs #-}

module Sandbox where

data Maybe' a = Nothing' | Just' a
  deriving Show

data Either' e a = Left' e | Right' a
  deriving Show

data Pair a b = Pair a b
  deriving Show

data UPair a = UPair a a
  deriving Show

instance Functor Maybe' where
  fmap :: (a -> b) -> Maybe' a -> Maybe' b
  fmap _   Nothing' = Nothing'
  fmap ab (Just' a) = Just' (ab a)

instance Functor (Either' e) where
  fmap :: (a -> b) -> Either' e a -> Either' e b
  fmap _  (Left'  e) = Left' e
  fmap ab (Right' a) = Right' $ ab a

instance Functor (Pair a) where
  fmap :: (b -> c) -> Pair a b -> Pair a c
  fmap f (Pair x y) = Pair x (f y)

instance Functor UPair where
  fmap :: (a -> b) -> UPair a -> UPair b
  fmap f (UPair x y) = UPair (f x) (f y)

