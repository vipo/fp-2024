import ClientDSL
import Network.Wreq
import Control.Lens ((^.))
import Data.String.Conversions (cs)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Control.Exception (try, SomeException)



main :: IO () -- defines a program using a DSL
main = do
    let program = do
            addAnimal "Lion" "Leo" 5
            addAnimal "Tiger" "Tina" 3
            addAnimal "Bug" "Sim" 1
            addAnimal "cat" "Murr" 4
            addAnimal "snake" "Long" 10
            deleteAnimal "cat" "Murr" 4
            addAnimal "dog" "Au" 8
            saveState
            listAnimals

    let batchRequest = "BEGIN\n" ++ interpretBatch program

    result <- sendBatchMain batchRequest
    case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right response -> putStrLn $ "\n\nResponse from server: " ++ response


sendBatchMain :: String -> IO (Either String String)
sendBatchMain batchRequest = do
    let url = "http://localhost:3000"
    let batchRequestBody = L.fromStrict (cs batchRequest :: B.ByteString)
    result <- try $ post url batchRequestBody :: IO (Either SomeException (Response L.ByteString))
    return $ case result of
        Left ex -> Left $ show ex
        Right response -> Right $ cs $ response ^. responseBody

