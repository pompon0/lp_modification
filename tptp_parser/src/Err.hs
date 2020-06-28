{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Err where

import Prelude hiding(fail)
import Control.Monad hiding(fail)
import Control.Monad.Fail
import Data.Functor.Identity
import qualified Data.Monoid as M

newtype Err a = Err (Either String a) deriving(Functor,Applicative,Monad,Show)
instance MonadFail Err where { fail = Err . Left }

when :: Monad m => Bool -> m () -> m ()
when True x = x
when False _ = return ()

err :: String -> Err a
err = Err . Left

(???) :: Err a -> String -> Err a
(???) (Err (Left msg)) ctx = Err (Left (ctx ++ "\n" ++ msg))
(???) x _ = x

r :: Monad m => a -> m a
r = return

-- s <- s & for [...] $ \a cont s -> do ...
for :: (Monad m, Foldable f) => f a -> (a -> (s -> m s) -> (s -> m s)) -> (s -> m s)
for c f = (M.appEndo $ foldMap (M.Endo . f) c) return

forI :: Monad m => [a] -> ((Int,a) -> (s -> m s) -> (s -> m s)) -> (s -> m s)
forI c f = for (zip [0..] c) f

while :: Monad m => (s -> Bool) -> (s -> m s) -> (s -> m s)
while p f s = if not (p s) then r s else while p f =<< f s

orFail :: MonadFail m => String -> (a -> m a) -> (Maybe a -> m (Maybe a))
orFail msg f ma = case ma of
  Nothing -> fail msg
  Just a -> Just <$> f a

class Monad m => MonadAssert m where
  assert :: MonadFail m' => m a -> m' a
instance MonadAssert Err where
  assert (Err (Left msg)) = fail msg
  assert (Err (Right x)) = return x
instance MonadAssert Maybe where
  assert (Nothing) = fail "Nothing"
  assert (Just x) = return x
instance MonadAssert Identity where
  assert (Identity x) = return x
