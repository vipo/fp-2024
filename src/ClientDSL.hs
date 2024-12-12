module ClientDSL where

import Control.Monad.Free
import Data.List (intercalate)
import Control.Exception (try, SomeException)



-- Define the DSL
data CommandDSL a
    = AddAnimal String String Int a
    | DeleteAnimal String String Int a
    | ListAnimals ([String] -> a)
    | SaveState a
    | LoadState (String -> a)

-- Functor instance for CommandDSL
instance Functor CommandDSL where
    fmap f (AddAnimal sp n a next) = AddAnimal sp n a (f next)
    fmap f (DeleteAnimal sp n a next) = DeleteAnimal sp n a (f next)
    fmap f (ListAnimals next) = ListAnimals (f . next)
    fmap f (SaveState next) = SaveState (f next)
    fmap f (LoadState next) = LoadState (f . next)

-- Free Monad wrapper
type Program = Free CommandDSL

-- DSL helper functions
addAnimal :: String -> String -> Int -> Program ()
addAnimal sp n a = liftF $ AddAnimal sp n a ()

deleteAnimal :: String -> String -> Int -> Program ()
deleteAnimal sp n a = liftF $ DeleteAnimal sp n a ()

listAnimals :: Program [String]
listAnimals = liftF $ ListAnimals id

saveState :: Program ()
saveState = liftF $ SaveState ()

loadState :: Program String
loadState = liftF $ LoadState id

interpretBatch :: Program a -> String
interpretBatch program = let (commands, _) = runBatch program in intercalate ";\n" commands ++ "\nEND"
  where
    runBatch :: Program a -> ([String], Maybe a)
    runBatch (Free (AddAnimal sp n a next)) =
        let (cmds, res) = runBatch next
         in (("ADD " ++ sp ++ " " ++ n ++ " " ++ show a) : cmds, res)
    runBatch (Free (DeleteAnimal sp n a next)) =
        let (cmds, res) = runBatch next
         in (("DELETE " ++ sp ++ " " ++ n ++ " " ++ show a) : cmds, res)
    runBatch (Free (ListAnimals _)) =
        (["LIST"], Nothing)
    runBatch (Free (SaveState next)) =
        let (cmds, res) = runBatch next
         in ("SAVE" : cmds, res)
    runBatch (Free (LoadState _)) =
        (["LOAD"], Nothing)
    runBatch (Pure a) = ([], Just a)

-- interpretOneByOne :: Program a -> IO [String]
-- interpretOneByOne (Free (AddAnimal sp n a next)) = do
--     sendBatch $ "ADD " ++ sp ++ " " ++ n ++ " " ++ show a
--     interpretOneByOne next
-- interpretOneByOne (Free (DeleteAnimal sp n a next)) = do
--     sendBatch $ "DELETE " ++ sp ++ " " ++ n ++ " " ++ show a
--     interpretOneByOne next
-- interpretOneByOne (Free (ListAnimals next)) = do
--     res <- sendBatch "LIST"
--     interpretOneByOne (next res)
-- interpretOneByOne (Free (SaveState next)) = do
--     sendBatch "SAVE"
--     interpretOneByOne next
-- interpretOneByOne (Free (LoadState next)) = do
--     res <- sendBatch "LOAD"
--     interpretOneByOne (next res)
-- interpretOneByOne (Pure _) = return []

-- sendBatch :: String -> IO String
-- sendBatch batchRequest = do
--     let url = "http://localhost:3000"
--     result <- try $ post url batchRequest :: IO (Either SomeException (Response L.ByteString))
--     return $ case result of
--         Left ex -> "Error: " ++ show ex
--         Right response -> unpack $ response ^. responseBody
