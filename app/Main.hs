{-# LANGUAGE FunctionalDependencies, FlexibleInstances, UndecidableInstances  #-}

module Main where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Monoid ((<>))

-- Interfaces

class Monad m => MonadState s m | m -> s where
  get :: m s
  put :: s -> m ()

class (Monoid l, Monad m) => MonadWriter l m | m -> l where
  tell :: l -> m ()

-- State

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance Monad m => Functor (StateT s m) where
  fmap f m = StateT $ \s -> do
    (x, s') <- runStateT m s
    return (f x, s')

instance Monad m => Applicative (StateT s m) where
  pure a = StateT $ \s -> return (a, s)
  mf <*> mx = StateT $ \s -> do
    (f, s') <- runStateT mf s
    (x, s'') <- runStateT mx s'
    return (f x, s'')

instance Monad m => Monad (StateT s m) where
  m >>= f = StateT $ \s -> do
    (x, s') <- runStateT m s
    runStateT (f x) s'

instance MonadIO m => MonadIO (StateT s m) where
  liftIO cmd = StateT $ \s -> do
    result <- liftIO cmd
    return (result, s)

instance Monad m => MonadState s (StateT s m) where
  get = StateT $ \s -> return (s, s)
  put s = StateT $ \_ -> return((), s)

instance MonadWriter l m => MonadWriter l (StateT s m) where
  tell l = StateT $ \s -> tell l >>= \x -> return (x, s)

-- Writer

newtype WriterT l m a = WriterT { runWriterT :: m (a, l) }

instance Monad m => Functor (WriterT l m) where
  fmap f m = WriterT $ do
    (x, l) <- runWriterT m
    return (f x, l)

instance (Monoid l, Monad m) => Applicative (WriterT l m) where
  pure x = WriterT $ return (x, mempty)
  mf <*> mx = WriterT $ do
    (f, l1) <- runWriterT mf
    (x, l2) <- runWriterT mx
    return (f x, l1 <> l2)

instance (Monoid l, Monad m) => Monad (WriterT l m) where
  m >>= f = WriterT $ do
    (x, l1) <- runWriterT m
    (y, l2) <- runWriterT (f x)
    return (y, l1 <> l2)

instance (Monoid l, MonadIO m) => MonadIO (WriterT l m) where
  liftIO cmd = WriterT $ do
    result <- liftIO cmd
    return (result, mempty)

instance (Monoid l, Monad m) => MonadWriter l (WriterT l m) where
  tell l = WriterT $ return ((), l)

instance (Monoid l, MonadState s m) => MonadState s (WriterT l m) where
  get = WriterT $ get >>= \x -> return (x, mempty)
  put s = WriterT $ put s >>= \x -> return (x, mempty)

-- Example

main :: IO ()
main = do
  ((x, logs), state) <- runStateT (runWriterT m) 2
  putStrLn $ "Sum = " ++ show x
  putStrLn $ "State = " ++ show state
  putStrLn $ "Logs = " ++ show logs
    where
      m :: WriterT String (StateT Int IO) Int
      m = do
        tell "Start\n"
        x <- get
        liftIO $ print x
        put (x + 1)
        y <- get
        tell "Before finish\n"
        liftIO $ print y
        tell "Finish\n"
        return $ x + y
