module Main where

import           Control.Concurrent      (forkIO, threadDelay)
import           Control.Concurrent.Chan
import           Control.Concurrent.STM
import           Control.Monad           (replicateM, unless)
import           Control.Monad.Fix       (fix)
import           System.IO               (hPutStrLn, stdout)
import           System.Random

import           SQueue
import           SStack

chPrint :: Chan String -> IO ()
chPrint chan = fix $ \f -> do
    str <- readChan chan
    hPutStrLn stdout str
    f

-- Basic Test
main :: IO ()
main = queueTest

stackTest = do
    queue <- atomically newSStack :: IO (SStack Int)
    chan <- newChan
    forkIO $ chPrint chan
    mapM_ forkIO $ replicate nOps (pop' chan queue)
    push' queue
    threadDelay 1000000
    hPutStrLn stdout "Finished."
     where
     nOps = 80000
     push' q = do nums <- replicateM nOps (randomRIO (1,20))
                  mapM_ (push q) nums
                  return ()
     pop' chan q = do num <- pop q
                      writeChan chan $  "Pop'd " ++ show num

queueTest = do
    queue <- atomically newSQueue :: IO (SQueue Int)
    chan <- newChan
    forkIO $ chPrint chan
    mapM_ forkIO $ replicate nOps (deq' chan queue)
    enq' queue
    threadDelay 1000000
    hPutStrLn stdout "Finished."
     where
     nOps = 80000
     enq' q = do nums <- replicateM nOps (randomRIO (1,20))
                 mapM_ (enqueue q) nums
                 return ()
     deq' chan q = do num <- deq q
                      writeChan chan $ "Deq'd " ++ show num
