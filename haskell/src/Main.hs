module Main where

import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TChan
import           Control.Monad                (replicateM, replicateM_, unless,
                                               when)
import           Control.Monad.Fix            (fix)
import           Data.Time.Clock
import           Options
import           System.IO                    (hPutStrLn, stdout)
import           System.Random

import           SQueue
import           SStack

data MainOptions = MainOptions
    { optNprods :: Int -- Number of produceres
    , optNconsm :: Int -- Number of consumers
    , optNops   :: Int -- Number of operations
    }

instance Options MainOptions where
    defineOptions = pure MainOptions
        <*> simpleOption "Nprods" 4
            "The number of producers"
        <*> simpleOption "Nconsm" 4
            "The number of consumers"
        <*> simpleOption "Nops" 500000
            "The number of operations to perform overall"

-- Basic Test
main :: IO ()
main = runCommand $ \opts args -> do
    let prods = optNprods opts
        consm = optNconsm opts
        nOps  = optNops opts
    c <- let x = getCurrentTime in x `seq` x 
    qwho prods consm nOps
    diff <- flip diffUTCTime c <$> getCurrentTime
    print diff

qwho nprod nconsm nops = do
    queue <- atomically newSQueue :: IO (SQueue Int)
    syncs <- atomically $ replicateM nprod newEmptyTMVar
    count <- atomically $ newTVar 0
    mapM_ (forkIO . enq' queue) syncs
    mapM_ forkIO $ replicate nconsm (deq' queue)

    mapM_ (atomically . takeTMVar) syncs
    return ()
    where
    enq' q mvar = do nums <- replicateM (nops `div` nprod) (randomRIO (1,20))
                     mapM_ (enqueue q) nums
                     atomically $ putTMVar mvar ()
    deq' q = fix $ \loop -> do
           num <- deq q
           loop
