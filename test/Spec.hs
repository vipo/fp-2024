{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (testProperty, (===), Arbitrary(..), arbitrary, shrink, oneof)
import Test.Tasty.QuickCheck as QC

import Data.List
import Data.Ord

import Lib1 qualified
import Lib2 qualified
import Lib3 qualified
import Control.Concurrent
import Data.Maybe

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

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
propertyTests :: TestTree
propertyTests = testGroup
  "Lib3 property tests"
  [
    QC.testProperty "Saved queries reproduce original state" savedQueriesReproduceState
  ]
--QC.testProperty "Save-then-load preserves state" saveThenLoadTest 
-- propertyTests :: TestTree
-- propertyTests = testGroup "Property Tests"
--   [ testProperty "Save-then-load preserves state" $
--       \statements -> saveThenLoadTest statements === Right (statements, "")
--   , testProperty "Rendering and parsing statements are consistent" $
--       \statements -> parseRenderConsistencyTest statements === Right (statements, "")
--   ]
instance Arbitrary Lib2.ArtType where
  arbitrary = oneof [pure Lib2.Painting, pure Lib2.Sculpture, pure Lib2.Digital, pure Lib2.Photograph, pure Lib2.Drawing, pure Lib2.Sketch]


-- Property test: Save-then-load consistency
saveThenLoadTest :: Lib2.State -> Property
saveThenLoadTest originalState = ioProperty $ do
    chan <- newChan
    let serializedState = Lib3.renderStatements (Lib3.marshallState originalState)
    saveReply <- newChan
    writeChan chan (Lib3.Save serializedState saveReply)
    _ <- readChan saveReply

    loadReply <- newChan
    writeChan chan (Lib3.Load loadReply)
    loadedState <- readChan loadReply

    pure $ serializedState == loadedState

-- Property test: Parsing rendered statements should yield the original
savedQueriesReproduceState :: Lib2.State -> Property
savedQueriesReproduceState originalState =
  let serialized = Lib3.renderStatements (Lib3.marshallState originalState)
      parsed = Lib3.parseStatements serialized
   in parsed === Right (Lib3.marshallState originalState, "")

-- Arbitrary instance for Lib2.Query
instance Arbitrary Lib2.Query where
  arbitrary = do
    artPiece <- arbitrary
    oneof
      [ return $ Lib2.AddArtwork artPiece
      , return $ Lib2.SellArtwork artPiece
      , return $ Lib2.PrintInfo
      , Lib2.Sequence <$> arbitrary
      ]
  shrink (Lib2.AddArtwork art) = Lib2.AddArtwork <$> shrink art
  shrink (Lib2.SellArtwork art) = Lib2.SellArtwork <$> shrink art
  shrink Lib2.PrintInfo = []
  shrink (Lib2.Sequence queries) = map Lib2.Sequence (shrink queries)

--Arbitrary instance for Lib2.ArtPiece
instance Arbitrary Lib2.ArtPiece where
  arbitrary = 
    Lib2.ArtPiece 
      <$> arbitrary  -- For artId
      <*> arbitrary  -- For title
      <*> arbitrary  -- For artType
      <*> arbitrary  -- For price
      <*> arbitrary  -- For description
  shrink art =
    [art {Lib2.artId = id'} | id' <- shrink (Lib2.artId art)]
    ++ [art {Lib2.title = title'} | title' <- shrink (Lib2.title art)]
    ++ [art {Lib2.price = price'} | price' <- shrink (Lib2.price art)]
    ++ [art {Lib2.description = desc'} | desc' <- shrink (Lib2.description art)]

instance Arbitrary Lib2.State where
  arbitrary = do
    artworks <- arbitrary -- List of artworks
    pure $ Lib2.State artworks -- Construct your state using appropriate fields
