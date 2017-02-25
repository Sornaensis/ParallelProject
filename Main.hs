module Main where

import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Monad                (replicateM, unless, when)
import           Control.Monad.Fix            (fix)
import           System.IO                    (hPutStrLn, stdout)
import           System.Random

import           SQueue
import           SStack

chPrint :: TChan String -> IO ()
chPrint chan = fix $ \loop -> do
    str <- atomically $ readTChan chan
    hPutStrLn stdout str
    loop

-- Basic Test
main :: IO ()
main = qwho

qwho = do
    queue <- atomically newSQueue :: IO (SQueue Int)
    chan <- newTChanIO
    count <- atomically $ newTVar 0
    forkIO $ chPrint chan
    mapM_ forkIO $ replicate nThreads (deq' count chan queue)
    mapM_ forkIO $ replicate nThreads (enq' queue)
    atomically $ do
        ct <- readTVar count
        unless (ct == nOps*nThreads) retry
    atomically $ do
        b <- isEmptyTChan chan
        unless b retry
    fct <- atomically $ readTVar count
    hPutStrLn stdout $ "Finished. Deq'd " ++ show fct ++ " times." 
     where
     nThreads = 8
     nOps = 80000
     enq' q = do nums <- replicateM nOps (randomRIO (1,20))
                 mapM_ (enqueue q) nums
                 return ()
     deq' ct chan q = fix $ \loop -> do
            num <- deq q
            atomically $ writeTChan chan $ "Deq'd " ++ show num
            atomically $ modifyTVar ct (+1)
            loop
