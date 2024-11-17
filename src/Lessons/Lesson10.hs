
module Lessons.Lesson10 () where
import Control.Applicative

import Control.Monad.Trans.State.Strict (State, StateT, get, put, runState, runStateT)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Monad.Trans.Class(lift)
import Control.Monad.IO.Class(liftIO)

type Parser a = ExceptT String (State String) a

parse :: Parser a -> String -> (Either String a, String)
parse parser = runState (runExceptT parser)

-- >>> parse (parseChar 'a') "aaaa"
-- (Right 'a',"aaa")
-- >>> parse (parseChar 'a') ""
-- (Left "Empty input","")
parseChar :: Char -> Parser Char
parseChar a = do
    input <- lift get
    case input of
        [] -> throwE "Empty input"
        (x:xs) -> if x == a
            then lift $ put xs >> return x
            else
                throwE $ a:" is not found"

-- >>> parse parseTwoAs "a a"
-- (Right (),"")
-- >>> parse parseTwoAs "a  a"
-- (Right (),"")
-- >>> parse parseTwoAs "aa"
-- (Left "  is not found","a")

parseTwoAs :: Parser ()
parseTwoAs = do
    _ <- parseChar 'a'
    _ <- some $ parseChar ' '
    _ <- parseChar 'a'
    return ()

-- >>> parse parseTwoAs' "a  a"
-- (Right (),"")
parseTwoAs' :: Parser ()
parseTwoAs' =
    (\ _ _ _ -> ()) <$> (parseChar 'a') <*> (some (parseChar ' ')) <*> (parseChar 'a')

-- >>> parse parseTwoAs'' "a  a"
-- (Right (),"")
parseTwoAs'' :: Parser ()
parseTwoAs'' = parseChar 'a' >> (some (parseChar ' ')) >> (parseChar 'a') >> return ()


type Weird a = ExceptT String (StateT Int IO) a

weird :: Weird Double
weird = do
    lift $ lift $ putStrLn "What is your name?"
    answer <- lift $ lift $ getLine
    lift $ put $ length answer
    return 3.14

weird' :: Weird Double
weird' = do
    liftIO $ putStrLn "What is your name?"
    answer <- lift $ lift $ getLine
    lift $ put $ length answer
    return 3.14