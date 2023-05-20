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
  fmap f (Just' a) = Just' (f a)

instance Applicative Maybe' where
  pure :: a -> Maybe' a
  pure x = Just' x

  (<*>) :: Maybe' (a -> b) -> Maybe' a -> Maybe' b
  Nothing' <*> _  = Nothing'
  Just' f  <*> mx = f <$> mx -- fmap f mx
  -- _ <*> Nothing'      = Nothing'
  -- Just' f <*> Just' x = Just' $ f x

instance Monad Maybe' where
  (>>=) :: Maybe' a -> (a -> Maybe' b) -> Maybe' b
  Nothing' >>= _   = Nothing'
  Just' a  >>= aMb = aMb a

instance Functor (Either' e) where
  fmap :: (a -> b) -> Either' e a -> Either' e b
  fmap _  (Left'  e) = Left' e
  fmap f  (Right' a) = Right' $ f a

instance Applicative (Either' e) where
  pure :: a -> Either' e a
  pure = Right'

  (<*>) :: Either' e (a -> b) -> Either' e a -> Either' e b
  Left' err <*> _  = Left' err
  Right' f  <*> ex = f <$> ex




instance Monad (Either' e) where
  (>>=) :: Either' e a -> (a -> Either' e b) -> Either' e b
  Left'  e >>= _   = Left' e
  Right' a >>= aEb = aEb a







instance Functor (Pair a) where
  fmap :: (b -> c) -> Pair a b -> Pair a c
  fmap f (Pair x y) = Pair x (f y)

instance Monoid a => Applicative (Pair a) where
  pure  :: b -> Pair a b
  pure x = Pair mempty x

  (<*>) :: Pair a (b -> c) -> Pair a b -> Pair a c
  Pair x f <*> Pair x' y = Pair (x <> x') (f y)

instance Functor UPair where
  fmap :: (a -> b) -> UPair a -> UPair b
  fmap f (UPair x y) = UPair (f x) (f y)

instance Applicative UPair where
  pure :: a -> UPair a
  pure x = UPair x x

  (<*>) :: UPair (a -> b) -> UPair a -> UPair b
  UPair f g <*> UPair x y = UPair (f x) (g y)