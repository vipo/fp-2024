import Web.Scotty
import qualified Lib2
import Control.Concurrent (forkIO)
import Data.String.Conversions (cs)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.Chan (newChan, Chan)
import Control.Concurrent.STM (newTVarIO, TVar)
import Lib3 ( StorageOp, storageOpLoop, parseCommand, stateTransition )


main :: IO ()
main = do
    stateVar <- newTVarIO Lib2.emptyState -- stateVar is a TVar, holds the application's shared state
    ioChan <- newChan -- ioChan - new channel for communication between threads
    _ <- forkIO $ storageOpLoop ioChan -- creates a new thread to run the storageOpLoop function which reads from ioChan and processes items

    scotty 3000 $ do -- scotty framework to start a server
        post (literal "/") $ do -- defines a POST endpoint at the root URL (/)
            bodyText <- body
            response <- liftIO $ processBatch stateVar ioChan (cs bodyText) -- request body to string
            text $ cs response -- response string back to text

processBatch :: TVar Lib2.State -> Chan StorageOp -> String -> IO String
processBatch stateVar ioChan input = case parseCommand input of -- parses input string into commands
    Left err -> return $ "Error parsing input: " ++ err
    Right (command, _) -> do
        result <- stateTransition stateVar command ioChan
        return $ case result of
            Left err -> "Error: " ++ err
            Right (msg, _) -> maybe "Success" id msg
