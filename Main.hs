module Main where

import           Control.Concurrent     (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Control.Monad          (replicateM, unless)
import           System.IO              (hPutStr, stdout)
import           System.Random

import           SQueue


-- Basic Test
main :: IO ()
main = do queue <- atomically newSQueue :: IO (SQueue Int)
          mapM_ forkIO $ replicate nOps (deq' queue)
          threadDelay 2000000
          enq' queue
          atomically $ flip unless retry =<< (isEmptySQueue queue)
          hPutStr stdout "Finished.\n"
     where
     nOps = 1000
     enq' q = do nums <- replicateM nOps (randomRIO (1,20))
                 mapM_ (enqueue q) nums
                 return ()
     deq' q = do num <- deq q
                 hPutStr stdout $ "Deq'd " ++ show num ++ "\n"
