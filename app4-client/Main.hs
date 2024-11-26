module Main (main) where

import Data.ByteString
import Network.Wreq
import Data.String.Conversions
import Control.Lens

main :: IO ()
main = do
    let rawRequest = cs "works?" :: ByteString
    resp <- post "http://localhost:3000" rawRequest
    putStrLn $ cs $ resp ^. responseBody