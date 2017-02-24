{-# LANGUAGE CPP #-}

module SQueue (
            SQueue
        ,   newSQueue
        ,   isEmptySQueue
        ,   enqueue
        ,   deq
        ) where

import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TVar
import           Control.Monad                (replicateM, unless)

-- This means that we want unboxed (non-heap) strict values
#define _UPK_(x) {-# UNPACK #-} !(x)

-- Abstract datatype to hide the guts of the queue
data SQueue a = SQueue _UPK_(TVar (TVarList a)) -- Head
                       _UPK_(TVar (TVarList a)) -- Tail
              deriving Eq

-- Use TMVars as boxes for synchronous boxes
type TVarList a = TVar (TMList a)
data TMList a = TNil | TCons (TBox a) _UPK_(TVarList a)

type TBox a = Either (TMVar a) (TMVar a)
--       Reservation ---^        ^---  Data

isEmptySQueue :: SQueue a -> STM Bool
isEmptySQueue (SQueue hptr _) = do
    h <- readTVar hptr
    head <- readTVar h
    case head of
        TNil -> return True
        TCons _ _ -> return False

-- Constructor
newSQueue :: STM (SQueue a)
newSQueue = do
    hole <- newTVar TNil
    h <- newTVar hole
    t <- newTVar hole
    return (SQueue h t)

-- Create a new TMVar and put it on the tail of the list
writeSQueue :: SQueue a -> (TMVar a -> TBox a) -> STM (TMVar a)
writeSQueue (SQueue _ tptr) boxf = do
    tail <- readTVar tptr
    new_tail <- newTVar TNil
    mvar <- newEmptyTMVar
    writeTVar tail (TCons (boxf mvar) new_tail)
    writeTVar tptr new_tail
    return mvar

writeReservation = flip writeSQueue Left
writeData = flip writeSQueue Right

-- Return a TMVar from the head of the list
getReservation :: SQueue a -> STM (TMVar a)
getReservation q@(SQueue hptr _) = do
    h <- readTVar hptr
    head <- readTVar h
    case head of
        TNil -> writeData q
        TCons mbox tail -> 
            case mbox of
                Left  mvar -> do
                    writeTVar hptr tail
                    return mvar
                Right _  -> writeData q

getData :: SQueue a -> STM (TMVar a)
getData q@(SQueue hptr _) = do
    h <- readTVar hptr
    head <- readTVar h
    case head of
        TNil -> writeReservation q
        TCons mbox tail -> 
            case mbox of
                Left  _    -> writeReservation q
                Right mvar -> do
                    writeTVar hptr tail
                    return mvar

-- API Function: blocking enqueue
enqueue :: SQueue a -> a -> IO ()
enqueue q a = do
    mvar <- atomically $ getReservation q
    atomically $ putTMVar mvar a
    atomically $ do
        fulfilled <- tryPutTMVar mvar a
        unless fulfilled retry

-- API Function: blocking deq
deq :: SQueue a -> IO a
deq q = do
    mvar <- atomically $ getData q
    atomically $ takeTMVar mvar

