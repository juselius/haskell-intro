module Main where
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import System.Exit

main :: IO ()
main = do
    lock <- atomically newEmptyTMVar
    shared <- atomically $ newTVar 0
    void . forkIO $ countup lock shared
    void . forkIO $ countdn lock shared
    forever $ do
        void . atomically $ takeTMVar lock
        val <- atomically $ readTVar shared
        when (val > 10) exitSuccess
        print val

countup :: TMVar () -> TVar Int -> IO ()
countup lock shared = forever $ do
    void . atomically $ count lock shared succ
    sleep 25

countdn :: TMVar () -> TVar Int -> IO ()
countdn lock shared = forever $ do
    void . atomically $ count lock shared pred
    sleep 35

count :: TMVar () -> TVar Int -> (Int -> Int) -> STM Int
count lock shared f = do
    val <- readTVar shared
    let v = f val
    writeTVar shared v
    putTMVar lock ()
    return v


sleep :: Int -> IO ()
sleep = threadDelay . (*) 10000
