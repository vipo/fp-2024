{-# LANGUAGE DeriveFunctor #-}
module Lessons.Lesson12 () where

import Control.Monad.Free (Free (..), liftF)
import Data.IORef
import qualified System.IO.Strict as S
import Control.Monad.Trans.State.Strict (State, StateT, get, put, runState, runStateT)

data MyDomainAlgebra next = Load (() -> next)
                          | Add Int (() -> next)
                          | Dump (String -> next)
                          | Save (() -> next)
                          deriving Functor

type MyDomain = Free MyDomainAlgebra

-- >>> id 5
-- 5
-- >>> id "labas"
-- "labas"
load :: MyDomain ()
load = liftF $ Load id

add :: Int -> MyDomain ()
add i = liftF $ Add i id

dump :: MyDomain String
dump = liftF $ Dump id

save :: MyDomain ()
save = liftF $ Save id

program :: MyDomain (String, String)
program = do
    load
    b <- dump
    add (1 + 1)
    add 10
    a <- dump
    save
    return (b, a)

runIOLog :: MyDomain a -> IO a
runIOLog (Pure a) = return a
runIOLog (Free step) = do
    next <- runStep step
    runIOLog next
    where
        runStep :: MyDomainAlgebra a -> IO a
        runStep (Load next) = do
            putStrLn "LOAD"
            return $ next ()
        runStep (Add i next) = do
            putStrLn $ "ADD " ++ show i
            return $ next ()
        runStep (Dump next) = do
            putStrLn $ "DUMPS"
            return $ next "fake state"
        runStep (Save next) = do
            putStrLn "SAVE"
            return $ next ()

runWithState :: MyDomain a -> State (Int, String) a
runWithState (Pure a) = return a
runWithState (Free step) = do
    next <- runStep step
    runWithState next
    where
        runStep :: MyDomainAlgebra a -> State (Int, String) a
        runStep (Load next) = do
            (_, f) <- get
            put (read f, f)
            return $ next ()
        runStep (Add i next) = do
            (s, f) <- get
            put (s + i, f)
            return $ next ()
        runStep (Dump next) = do
            (_, f) <- get
            return $ next f
        runStep (Save next) = do
            (s, _) <- get
            put (s, show s)
            return $ next ()

runIO :: MyDomain a -> IO a
runIO p = do
    v <- newIORef 0
    runIO' v p
    where
        runIO' :: IORef Int ->  MyDomain a -> IO a
        runIO' _ (Pure a) = return a
        runIO' v (Free step) = do
            next <- runStep v step
            runIO' v next
        runStep :: IORef Int -> MyDomainAlgebra a -> IO a
        runStep v (Load next) = do
            a <- S.readFile "./data"
            writeIORef v (read a)
            return $ next ()
        runStep v (Add i next) = do
            a <- readIORef v
            writeIORef v (a + i)
            return $ next ()
        runStep v (Dump next) = do
            a <- readIORef v
            return $ next $ show a
        runStep v (Save next) = do
            a <- readIORef v
            writeFile "./data" (show a)
            return $ next ()
