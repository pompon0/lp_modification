{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Err where

import Prelude hiding(fail)
import Control.Monad hiding(fail)
import Control.Monad.Fail
import qualified Data.Monoid as M

newtype Err a = Err (Either String a) deriving(Functor,Applicative,Monad)
instance MonadFail Err where { fail = Err . Left }

when :: Monad m => Bool -> m () -> m ()
when True x = x
when False _ = return ()

r :: Monad m => a -> m a
r = return

-- s <- s & for [...] $ \a cont s -> do ...
for :: (Monad m, Foldable f) => f a -> (a -> (s -> m s) -> (s -> m s)) -> (s -> m s)
for c f = (M.appEndo $ foldMap (M.Endo . f) c) return

orFail :: MonadFail m => String -> (a -> m a) -> (Maybe a -> m (Maybe a))
orFail msg f ma = case ma of
  Nothing -> fail msg
  Just a -> Just <$> f a

while :: Monad m => (s -> Bool) -> (s -> m s) -> (s -> m s)
while p f s = if p s then r s else while p f =<< f s

