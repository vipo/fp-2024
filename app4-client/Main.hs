import ClientDSL
import Network.Wreq
import Control.Lens ((^.))
import Data.String.Conversions (cs)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Control.Exception (try, SomeException)


-- Makes a batch of commands using DSL
-- and with HTTP POST request sends them
-- to a server and processes the response.
main :: IO ()
main = do
    let program = do -- do chans monadic opperations together
            addAnimal "cat" "Mickus" 4
            addAnimal "lion" "Leo" 5
            addAnimal "tiger" "Tina" 3
            addAnimal "bug" "Sim" 1
            addAnimal "cat" "Murr" 4
            addAnimal "snake" "Long" 10
            deleteAnimal "cat" "Murr" 4
            addAnimal "dog" "Au" 8
            saveState
            listAnimals

    let batchRequest = "BEGIN\n" ++ interpretBatch program

    -- sends received commands (string) to a server
    result <- sendBatchMain batchRequest
    case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right response -> putStrLn $ "\n\nResponse from server: " ++ response


sendBatchMain :: String -> IO (Either String String)
sendBatchMain batchRequest = do
    let url = "http://localhost:3000"
    let batchRequestBody = L.fromStrict (cs batchRequest :: B.ByteString) -- string to Lazy ByteSting format
    result <- try $ post url batchRequestBody :: IO (Either SomeException (Response L.ByteString)) -- tries to send
    return $ case result of
        Left ex -> Left $ show ex
        Right response -> Right $ cs $ response ^. responseBody -- converts response of Lazy ByteString format back to String






-- -- for one by one command
-- {-# LANGUAGE OverloadedStrings #-}

-- module Main where

-- import ClientDSL
-- import Network.Wreq
-- import Control.Lens ((^.))
-- import Control.Exception (try, SomeException)
-- import qualified Data.ByteString.Lazy.Char8 as L

-- sendBatch :: String -> IO String
-- sendBatch batchRequest = do
--     let url = "http://localhost:3000"
--     let batchRequestBody = L.pack batchRequest
--     result <- try $ post url batchRequestBody :: IO (Either SomeException (Response L.ByteString))
--     case result of
--         Left ex -> do
--             putStrLn $ "Error sending request: " ++ show ex
--             return "Error"
--         Right response -> do
--             let res = L.unpack (response ^. responseBody)
--             putStrLn $ "Server response: " ++ res
--             return res


-- main :: IO ()
-- main = do
--     putStrLn "Starting Client..."
--     let program = do
--             addAnimal "Lion" "Leo" 5
--             addAnimal "Tiger" "Tina" 3
--             listAnimals
--             saveState
--             loadState

--     -- Execute program step by step
--     result <- interpretOneByOne program
--     putStrLn "Commands executed one-by-one."
--     putStrLn "Final Results:"
--     print result
