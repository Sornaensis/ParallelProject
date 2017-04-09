{-# LANGUAGE CPP #-}

module SQueue (
            SQueue
        ,   newSQueue
        ,   isEmptySQueue
        ,   enqueue
        ,   deq
        ) where

import           Control.Concurrent.STM

-- This means that we want unpacked strict values
#define _UPK_(x) {-# UNPACK #-} !(x)

-- Abstract datatype to hide the guts of the queue
data SQueue a = SQueue _UPK_(TVar (TVarList a)) -- Head
                       _UPK_(TVar (TVarList a)) -- Tail
              deriving Eq

type TVarList a = TVar (TMList a)
data TMList a = TNil 
              | TCons (TBox a) _UPK_(TVarList a)

-- Use TMVars as boxes for synchronous boxes
data TBox a = Reservation _UPK_(TMVar a)
            | Data        _UPK_(TMVar a)

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

writeSQueue :: SQueue a -> (TMVar a -> TBox a) -> STM (TMVar a)
writeSQueue (SQueue _ tptr) boxf = do
    tail <- readTVar tptr
    new_tail <- newTVar TNil
    mvar <- newEmptyTMVar
    writeTVar tail (TCons (boxf mvar) new_tail)
    writeTVar tptr new_tail
    return mvar

-- Writing reservations and data is symmetric, just
-- pass in the correct TBox constructor
writeReservation :: SQueue a -> STM (TMVar a)
writeReservation = flip writeSQueue Reservation

writeData :: SQueue a -> STM (TMVar a)
writeData = flip writeSQueue Data

-- Return a TMVar from the head of the queue if it 
-- is a reservation.
-- Else, write data to the end of the queue
getReservation :: SQueue a -> STM (TMVar a)
getReservation q@(SQueue hptr _) = do
    h <- readTVar hptr
    head <- readTVar h
    case head of
        TNil -> writeData q
        TCons mbox tail ->
            case mbox of
                Reservation  mvar -> do
                    writeTVar hptr tail
                    return mvar
                Data         _  -> writeData q

-- Opposite of `getReservation`
getData :: SQueue a -> STM (TMVar a)
getData q@(SQueue hptr _) = do
    h <- readTVar hptr
    head <- readTVar h
    case head of
        TNil -> writeReservation q
        TCons mbox tail ->
            case mbox of
                Reservation  _    -> writeReservation q
                Data         mvar -> do
                    writeTVar hptr tail
                    return mvar

-- API Function: blocking enqueue
enqueue :: SQueue a -> a -> IO ()
enqueue q a = do
    mvar <- atomically $ getReservation q
    atomically $ putTMVar mvar a
    atomically $ putTMVar mvar a

-- API Function: blocking deq
deq :: SQueue a -> IO a
deq q = do
    mvar <- atomically $ getData q
    atomically $ takeTMVar mvar
