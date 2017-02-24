{-# LANGUAGE CPP #-}

module SQueue (
            SQueue
        ,   newSQueue
        ,   isEmptySQueue
        ,   enqueue
        ,   deq
        ) where

import           Control.Concurrent.STM
import           Control.Monad                (unless)

-- This means that we want unboxed (non-heap) strict values
#define _UPK_(x) {-# UNPACK #-} !(x)

-- Abstract datatype to hide the guts of the queue
data SQueue a = SQueue _UPK_(TVar (TVarList a)) -- Head
                       _UPK_(TVar (TVarList a)) -- Tail
              deriving Eq

type TVarList a = TVar (TMList a)
data TMList a = TNil | TCons (TBox a) _UPK_(TVarList a)

-- Use TMVars as boxes for synchronous boxes
-- Sum Type differentiating Reservations and Data boxes
data TBox a = Reservation _UPK_(TMVar a)  -- Reservation TMVar
            | Data        _UPK_(TMVar a)  -- Data        TMVar

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

-- Return a TMVar from the head of the queue if it
-- contains data.
-- Else, write a reservation to the end of the queue
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

{- 
 -- API Functions exposed by this module --
 -
 - `enqueue` and `deq` both work inside the IO monad. The reason for this is
 - to allow us to construct sequential atomic operations for a single thread.
 - 
 - e.g., a single thread running enqueue will first either retrieve a reservation
 - TMVar from the queue or place a data TMVar at the end of the queue and this action
 - cannot be interrupted by any other thread.
 -      This thread will then atomically FILL this TMVar, which is either in the queue 
 - waiting to be removed by a call to deq or already removed and connected to a blocking 
 - deq. The thread then attempts to fill the TMVar again, which may only occur when the TMVar
 - has been emptied by the blocking deq. If it fails it retries, thus blocking until the deq
 - thread consumes the contents of the TMVar.
 -
 - -}

-- API Function: blocking enqueue
enqueue :: SQueue a -> a -> IO ()
enqueue q a = do
    mvar <- atomically $ getReservation q
    atomically $ putTMVar mvar a
    -- atomically $ putTMVar mvar a -- | TODO: Benchmark this instead of the code below
    atomically $ do
        fulfilled <- tryPutTMVar mvar a
        unless fulfilled retry

-- | TODO: Time-out enqueue
enqueue_ :: SQueue a -> a -> IO Bool
enqueue_ = undefined

-- API Function: blocking deq
deq :: SQueue a -> IO a
deq q = do
    mvar <- atomically $ getData q
    atomically $ takeTMVar mvar

-- | TODO: Time-out deq
deq_ :: SQueue a -> IO (Maybe a)
deq_ _ = undefined
