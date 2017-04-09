module Main where

import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Monad                (replicateM, unless, when)
import           Control.Monad.Fix            (fix)
import           Options
import           System.IO                    (hPutStrLn, stdout)
import           System.Random

import           SQueue
import           SStack

-- Basic Test
main :: IO ()
main = qwho

qwho = do
    queue <- atomically newSQueue :: IO (SQueue Int)
    count <- atomically $ newTVar 0
    mapM_ forkIO $ replicate nThreads (deq' count queue)
    mapM_ forkIO $ replicate nThreads (enq' queue)
    atomically $ do
        ct <- readTVar count
        unless (ct == nOps*nThreads) retry
    fct <- atomically $ readTVar count
    hPutStrLn stdout $ "Finished. Deq'd " ++ show fct ++ " times."
     where
     nThreads = 4
     nOps = 2*80000
     enq' q = do nums <- replicateM nOps (randomRIO (1,20))
                 mapM_ (enqueue q) nums
                 return ()
     deq' ct q = fix $ \loop -> do
            num <- deq q
            atomically $ modifyTVar' ct (+1)
            loop
