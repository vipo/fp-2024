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
  [ testCase "Add Artwork successfully" $ do
      let initialState = Lib2.emptyState
      let artwork = Lib2.ArtPiece 1 "Starry Night" Lib2.Painting 2000 "A famous painting."
      case Lib2.stateTransition initialState (Lib2.AddArtwork artwork) of
        Left err -> assertFailure err
        Right (msg, newState) -> do
          msg @?= Just "Artwork added."
          length (Lib2.artworks newState) @?= 1  -- Verify the artwork was added.

  , testCase "Add duplicate Artwork" $ do
      let initialState = Lib2.emptyState
      let artwork = Lib2.ArtPiece 1 "Starry Night" Lib2.Painting 2000 "A famous painting."
      -- First add the artwork.
      case Lib2.stateTransition initialState (Lib2.AddArtwork artwork) of
        Left err -> assertFailure err
        Right (_, stateAfterAdd) -> do
          -- Try to add the same artwork again.
          case Lib2.stateTransition stateAfterAdd (Lib2.AddArtwork artwork) of
            Left err -> err @?= "Artwork with the same ID already exists."
            Right _ -> assertFailure "Expected an error for duplicate artwork."

  , testCase "Sell Artwork successfully" $ do
      let initialState = Lib2.emptyState
      let artwork = Lib2.ArtPiece 1 "Starry Night" Lib2.Painting 2000 "A famous painting."
      -- First add the artwork.
      case Lib2.stateTransition initialState (Lib2.AddArtwork artwork) of
        Left err -> assertFailure err
        Right (_, stateAfterAdd) -> do
          -- Now sell the artwork.
          case Lib2.stateTransition stateAfterAdd (Lib2.SellArtwork artwork) of
            Left err -> assertFailure err
            Right (msg, finalState) -> do
              msg @?= Just "Artwork sold."
              length (Lib2.artworks finalState) @?= 0  -- Verify the artwork was sold.

  , testCase "Sell non-existent Artwork" $ do
      let initialState = Lib2.emptyState
      let artwork = Lib2.ArtPiece 1 "Starry Night" Lib2.Painting 2000 "A famous painting."
      case Lib2.stateTransition initialState (Lib2.SellArtwork artwork) of
        Left err -> err @?= "Artwork not found for sale."
        Right _ -> assertFailure "Expected an error when selling non-existent artwork."

  , testCase "Print info on empty state" $ do
      let initialState = Lib2.emptyState
      case Lib2.stateTransition initialState Lib2.PrintInfo of
        Left err -> assertFailure err
        Right (msg, _) -> msg @?= Just "No artworks available."

  , testCase "Print info after adding artwork" $ do
      let initialState = Lib2.emptyState
      let artwork = Lib2.ArtPiece 1 "Starry Night" Lib2.Painting 2000 "A famous painting."
      case Lib2.stateTransition initialState (Lib2.AddArtwork artwork) of
        Left err -> assertFailure err
        Right (_, stateAfterAdd) -> do
          case Lib2.stateTransition stateAfterAdd Lib2.PrintInfo of
            Left err -> assertFailure err
            Right (msg, _) -> msg @?= Just "ArtPiece {artId = 1, title = \"Starry Night\", artType = Painting, price = 2000.0, description = \"A famous painting.\"}"

  , testCase "Sequence of operations" $ do
      let initialState = Lib2.emptyState
      let artwork1 = Lib2.ArtPiece 1 "Starry Night" Lib2.Painting 2000 "A famous painting."
      let artwork2 = Lib2.ArtPiece 2 "The Night Cafe" Lib2.Painting 1500 "Another famous painting."
      case Lib2.stateTransition initialState (Lib2.Sequence 
            [ Lib2.AddArtwork artwork1
            , Lib2.AddArtwork artwork2
            , Lib2.PrintInfo
            , Lib2.SellArtwork artwork1
            , Lib2.PrintInfo
            ]) of
        Left err -> assertFailure err
        Right (msg, finalState) -> do
          msg @?= Just "ArtPiece {artId = 1, title = \"Starry Night\", artType = Painting, price = 2000.0, description = \"A famous painting.\"}\nArtPiece {artId = 2, title = \"The Night Cafe\", artType = Painting, price = 1500.0, description = \"Another famous painting.\"}"
          length (Lib2.artworks finalState) @?= 1  -- Verify one artwork is left.

  , testCase "Sell already sold Artwork" $ do
      let initialState = Lib2.emptyState
      let artwork = Lib2.ArtPiece 1 "Starry Night" Lib2.Painting 2000 "A famous painting."
      -- Add and sell the artwork.
      case Lib2.stateTransition initialState (Lib2.AddArtwork artwork) of
        Left err -> assertFailure err
        Right (_, stateAfterAdd) -> do
          case Lib2.stateTransition stateAfterAdd (Lib2.SellArtwork artwork) of
            Left err -> assertFailure err
            Right (_, stateAfterSell) -> do
              -- Try to sell it again.
              case Lib2.stateTransition stateAfterSell (Lib2.SellArtwork artwork) of
                Left err -> err @?= "Artwork not found for sale."
                Right _ -> assertFailure "Expected an error when selling an already sold artwork."

  , testCase "Adding multiple artworks and checking state" $ do
      let initialState = Lib2.emptyState
      let artwork1 = Lib2.ArtPiece 1 "Starry Night" Lib2.Painting 2000 "A famous painting."
      let artwork2 = Lib2.ArtPiece 2 "The Night Cafe" Lib2.Painting 1500 "Another famous painting."
      let artwork3 = Lib2.ArtPiece 3 "Mona Lisa" Lib2.Painting 2500 "The most famous painting."
      
      let (msg1, stateAfterAdd1) = Lib2.stateTransition initialState (Lib2.AddArtwork artwork1)
      msg1 @?= Just "Artwork added."

      let (msg2, stateAfterAdd2) = Lib2.stateTransition stateAfterAdd1 (Lib2.AddArtwork artwork2)
      msg2 @?= Just "Artwork added."

      let (msg3, stateAfterAdd3) = Lib2.stateTransition stateAfterAdd2 (Lib2.AddArtwork artwork3)
      msg3 @?= Just "Artwork added."

      length (Lib2.artworks stateAfterAdd3) @?= 3  -- Verify three artworks were added.
  ]
