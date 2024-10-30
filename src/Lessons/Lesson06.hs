module Lessons.Lesson06 () where
import Control.Concurrent (newChan, threadDelay, forkIO, 
    readChan, writeChan, Chan)
import System.Random
import Control.Concurrent.STM (TVar)
import Control.Monad.STM
import Control.Concurrent.STM.TVar (readTVar, readTVarIO)
import Control.Concurrent.STM (writeTVar)
import Control.Concurrent.Async
import Control.Monad (when)
import Control.Concurrent.STM (newTVarIO)
import Control.Concurrent.STM (modifyTVar)


action :: String -> IO ()
action name = do
    i <- randomIO :: IO Int
    let sleep = abs $ i `mod` 5000000
    putStr $ "Sleeping for " ++ show sleep ++ " microseconds in " ++ name
    threadDelay sleep
    putStr $ "Done " ++ name

runInPar:: IO ()
runInPar = do
    _ <- forkIO $ action "one"
    _ <- forkIO $ action "two"
    return ()

action' :: String -> Chan String -> IO ()
action' name chan = do
    i <- randomIO :: IO Int
    let sleep = abs $ i `mod` 5000000
    threadDelay sleep
    writeChan chan $ "Slept for " ++ show sleep ++ " microseconds in " ++ name

chanReader :: Chan String -> IO ()
chanReader chan = do
    v <- readChan chan
    putStrLn v
    chanReader chan

runInPar' :: IO ()
runInPar' = do
    chan <- newChan
    _ <- forkIO $ action' "one" chan
    _ <- forkIO $ action' "two" chan
    chanReader chan
    return ()

action'' :: String -> IO String
action'' name = do
    i <- randomIO :: IO Int
    let sleep = abs $ i `mod` 5000000
    threadDelay sleep
    return $ "Slept for " ++ show sleep ++ " microseconds in " ++ name

asyncExample :: IO ()
asyncExample = do
    c1 <- async $ action'' "one"
    c2 <- async $ action'' "two"
    v1 <- wait c1
    v2 <- wait c2
    putStrLn $ v1 ++ " " ++ v2
    return ()

transfer :: TVar Integer -> TVar Integer -> Integer -> STM ()
transfer accA accB amount = do
    a <- readTVar accA
    b <- readTVar accB
    writeTVar accA (a - amount)
    writeTVar accB (b + amount)
    newAValue <- readTVar accA
    when (newAValue < 0) retry

tryTransfer :: IO ()
tryTransfer = do
    accA <- newTVarIO 50
    accB <- newTVarIO 100
    chan <- newChan
    forkIO $ do
        atomically $ transfer accA accB 200
        writeChan chan ()
    atomically $ modifyTVar accA (+ 150)
    _ <- readChan chan
    a <- readTVarIO accA
    b <- readTVarIO accB
    putStrLn $ "A: " ++ show a ++ ", " ++ "B: " ++ show b
