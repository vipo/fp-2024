{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=), assertBool, assertFailure )

import Lib1 qualified
import Lib2 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Lib2 tests"
  [
    testGroup "Parser Tests"
      [ testCase "parseAddArtwork" $
          Lib2.parseAddArtwork "add_artwork(1 \"Title\" Painting 100.0 \"Description\")"
            @?= Right (Lib2.AddArtwork (Lib2.ArtPiece 1 "Title" Lib2.Painting 100.0 "Description"), "")

      , testCase "parseSellArtwork" $
          Lib2.parseSellArtwork "sell_artwork(1 \"Title\" Painting 100.0 \"Description\")"
            @?= Right (Lib2.SellArtwork (Lib2.ArtPiece 1 "Title" Lib2.Painting 100.0 "Description"), "")

      , testCase "parsePrintInfo" $
          Lib2.parsePrintInfo "print_info()" @?= Right (Lib2.PrintInfo, "")

      , testCase "parseCommands Sequence" $
          Lib2.parseCommands "add_artwork(1 \"Title\" Painting 100.0 \"Description\"); print_info()" 
            @?= Right ([Lib2.AddArtwork (Lib2.ArtPiece 1 "Title" Lib2.Painting 100.0 "Description"), Lib2.PrintInfo], "")
      ]
    
    , testGroup "State Transition Tests"
      [ testCase "Add Artwork" $
          let initialState = Lib2.emptyState
              artPiece = Lib2.ArtPiece 1 "Title" Lib2.Painting 100.0 "Description"
              query = Lib2.AddArtwork artPiece
          in Lib2.stateTransition initialState query
             @?= Right (Just "Artwork added.", Lib2.State { Lib2.artworks = [artPiece] })

      , testCase "Sell Artwork (Artwork exists)" $
          let initialState = Lib2.State { Lib2.artworks = [Lib2.ArtPiece 1 "Title" Lib2.Painting 100.0 "Description"] }
              artPiece = Lib2.ArtPiece 1 "Title" Lib2.Painting 100.0 "Description"
              query = Lib2.SellArtwork artPiece
          in Lib2.stateTransition initialState query
             @?= Right (Just "Artwork sold.", Lib2.State { Lib2.artworks = [] })

      , testCase "Sell Artwork (Artwork does not exist)" $
          let initialState = Lib2.emptyState
              artPiece = Lib2.ArtPiece 1 "Title" Lib2.Painting 100.0 "Description"
              query = Lib2.SellArtwork artPiece
          in Lib2.stateTransition initialState query
             @?= Left "Artwork not found for sale."

      , testCase "Print Info (No Artworks)" $
          Lib2.stateTransition Lib2.emptyState Lib2.PrintInfo @?= Right (Just "No artworks available.", Lib2.emptyState)

      , testCase "Print Info (With Artworks)" $
          let stateWithArt = Lib2.State { Lib2.artworks = [Lib2.ArtPiece 1 "Title" Lib2.Painting 100.0 "Description"] }
          in Lib2.stateTransition stateWithArt Lib2.PrintInfo
             @?= Right (Just (unlines ["ArtPiece {artId = 1, title = \"Title\", artType = Painting, price = 100.0, description = \"Description\"}"]), stateWithArt)

      , testCase "Sequence of Commands" $
          let initialState = Lib2.emptyState
              artPiece = Lib2.ArtPiece 1 "Title" Lib2.Painting 100.0 "Description"
              query = Lib2.Sequence [Lib2.AddArtwork artPiece, Lib2.PrintInfo]
          in Lib2.stateTransition initialState query
             @?= Right (Just "Artwork added.\nArtPiece {artId = 1, title = \"Title\", artType = Painting, price = 100.0, description = \"Description\"}", Lib2.State { Lib2.artworks = [artPiece] })
      ]
  ]
