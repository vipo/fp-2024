{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad.IO.Class(liftIO)
import Data.String.Conversions
import Web.Scotty

main :: IO ()
main = scotty 3000 $
    post "/" $ do
        b <- body
        liftIO $ putStrLn $ concat ["Request was: ", cs b]
        text $ "This is response"
