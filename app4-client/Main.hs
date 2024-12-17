{-# LANGUAGE DeriveFunctor #-}

module Main (main) where

import Data.ByteString
import Network.Wreq
import Data.String.Conversions
import Control.Lens 
import Control.Monad.Free (Free (..), liftF)

data MyDomainAlgebra next
  = View (String -> next)
  | Load (String -> next)
  | Save (String -> next)
  | Delete (String -> next)
  | Add String (String -> next) 
  deriving (Functor)

type DeckProgram = Free MyDomainAlgebra

view :: DeckProgram String
view = liftF $ View id

load :: DeckProgram String
load = liftF $ Load id

save :: DeckProgram String
save = liftF $ Save id

delete :: DeckProgram String
delete = liftF $ Delete id

add :: String -> DeckProgram String
add s = liftF $ Add s id



oneByOne :: DeckProgram a -> IO (String, a)
oneByOne = runStep ""
  where
    runStep acc (Pure a) = return (acc, a)
    runStep acc (Free (View next)) = do
      putStrLn "Sending request: view"
      resp <- post "http://localhost:3000" (cs "view" :: ByteString)
      let responseStr = cs $ resp ^. responseBody
      runStep (acc ++ "View Response: " ++ responseStr ++ "\n") (next responseStr)
    runStep acc (Free (Load next)) = do
      putStrLn "Sending request: load"
      resp <- post "http://localhost:3000" (cs "load" :: ByteString)
      let responseStr = cs $ resp ^. responseBody
      runStep (acc ++ "Load Response: " ++ responseStr ++ "\n") (next responseStr)
    runStep acc (Free (Save next)) = do
      putStrLn "Sending request: save"
      resp <- post "http://localhost:3000" (cs "save" :: ByteString)
      let responseStr = cs $ resp ^. responseBody
      runStep (acc ++ "Load Response: " ++ responseStr ++ "\n") (next responseStr)
    runStep acc (Free (Delete next)) = do
      putStrLn "Sending request: delete"
      resp <- post "http://localhost:3000" (cs "delete" :: ByteString)
      let responseStr = cs $ resp ^. responseBody
      runStep (acc ++ "Load Response: " ++ responseStr ++ "\n") (next responseStr)
    runStep acc (Free (Add s next)) = do
      putStrLn ("Adding: " ++ s)
      resp <- post "http://localhost:3000" (cs ("add " ++ s) :: ByteString)
      let responseStr = cs $ resp ^. responseBody
      runStep (acc ++ "Add Response: " ++ responseStr ++ "\n") (next responseStr)


program :: DeckProgram String
program = do
    Main.delete
    load
    add "Two of Spades, Joker"
    Main.view
    


main :: IO ()
main = do
    (responses, _) <- oneByOne program
    putStrLn "Accumulated Responses:"
    putStrLn responses