{-# LANGUAGE CPP #-}

module SStack (
            SStack
        ,   newSStack
        ,   isEmptySStack
        ,   push
        ,   pop
        ) where

import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TVar
import           Control.Monad                (replicateM, unless)

-- This means that we want unboxed (non-heap) strict values
#define _UPK_(x) {-# UNPACK #-} !(x)

-- Abstract datatype to hide the guts of the queue
data SStack a = SStack _UPK_(TVar (TVarList a)) -- Head
              deriving Eq

-- Use TMVars as boxes for synchronous boxes
type TVarList a = TVar (TMList a)
data TMList a = TNil | TCons (TBox a) _UPK_(TVarList a)

type TBox a = Either (TMVar a) (TMVar a)
--       Reservation ---^        ^---  Data

isEmptySStack :: SStack a -> STM Bool
isEmptySStack (SStack hptr) = do
    h <- readTVar hptr
    head <- readTVar h
    case head of
        TNil -> return True
        TCons _ _ -> return False

-- Constructor
newSStack :: STM (SStack a)
newSStack = do
    hole <- newTVar TNil
    h <- newTVar hole
    return (SStack h)

-- Create a new TMVar and put it on the tail of the list
writeSStack :: SStack a -> (TMVar a -> TBox a) -> STM (TMVar a)
writeSStack (SStack hptr) boxf = do
    head <- readTVar hptr
    new_head <- newTVar TNil
    mvar <- newEmptyTMVar
    writeTVar new_head (TCons (boxf mvar) head)
    writeTVar hptr new_head
    return mvar

writeReservation = flip writeSStack Left
writeData = flip writeSStack Right

-- Return a TMVar from the head of the list
getReservation :: SStack a -> STM (TMVar a)
getReservation q@(SStack hptr) = do
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

getData :: SStack a -> STM (TMVar a)
getData q@(SStack hptr) = do
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
push :: SStack a -> a -> IO ()
push q a = do
    mvar <- atomically $ getReservation q
    atomically $ putTMVar mvar a
    atomically $ do
        fulfilled <- tryPutTMVar mvar a
        unless fulfilled retry

-- API Function: blocking deq
pop :: SStack a -> IO a
pop q = do
    mvar <- atomically $ getData q
    atomically $ takeTMVar mvar

