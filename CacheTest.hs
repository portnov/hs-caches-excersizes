{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

import Control.Monad
import Control.Concurrent
import qualified Data.Map as M
import Data.IORef
import qualified Data.Text as T
import System.Random
import System.Mem.Weak
import Text.Printf
import Criterion
import Criterion.Main

type Key = T.Text

data Entity = Entity Key T.Text
  deriving (Eq, Show)

getKey :: Entity -> Key
getKey (Entity k _) = k

class Cache c where
  put :: c -> Entity -> IO ()
  get :: c -> Key -> IO (Maybe Entity)

type NaiveCache = IORef (M.Map Key Entity)

createNaiveCache :: IO NaiveCache
createNaiveCache = newIORef M.empty

type WeakCache = IORef (M.Map Key (Weak Entity))

createWeakCache :: IO WeakCache
createWeakCache = newIORef M.empty

instance Cache (IORef (M.Map Key Entity)) where
  
  put cache entity = modifyIORef cache $ M.insert (getKey entity) entity

  get cache key = do
    map <- readIORef cache
    return $ M.lookup key map

instance Cache (IORef (M.Map Key (Weak Entity))) where

  put cache entity = do
    let key = getKey entity
    ref <- mkWeakPtr entity Nothing
    modifyIORef cache $ M.insert key ref

  get cache key = do
    map <- readIORef cache
    case M.lookup key map of
      Nothing -> return Nothing
      Just ref -> do
        deRefWeak ref

data_size :: Int
data_size = 10000

gets :: Int
gets = 5000

max_key :: Int
max_key = 12000

doTest :: Cache cache => cache -> IO ()
doTest cache = do
  forM_ [1..data_size] $ \i -> do
    let key = T.pack $ show i
        entity = Entity key $ T.pack $ show i
    put cache entity

  successes <- newIORef (0 :: Int)
  misses <- newIORef (0 :: Int)

  forM_ [1..gets] $ \_ -> do
    k <- randomRIO (1, max_key)
    let key = T.pack $ show k
    r <- get cache key
    case r of
      Nothing -> do
          threadDelay $ 5 * 1000
          modifyIORef misses (+1)
      Just _ -> modifyIORef successes (+1)

  s <- readIORef successes
  m <- readIORef misses
  printf "Successes: %d; Misses: %d\n" s m

naiveTest :: IO ()
naiveTest = do
  doTest =<< createNaiveCache

weakTest :: IO ()
weakTest = do
  doTest =<< createWeakCache

main :: IO ()
main = defaultMain [
         bench "naive" $ nfIO naiveTest,
         bench "weak" $ nfIO weakTest
       ]

