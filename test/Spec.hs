{-# LANGUAGE ImportQualifiedPost #-}

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck as QC
import Lib2 qualified
import Lib3 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [propertyTests]

-- Property Tests
propertyTests :: TestTree
propertyTests = testGroup "Property Tests"
  [ 
    QC.testProperty "Saves, loads and preserves state" $ withMaxSuccess 1 saveThenLoad
  , QC.testProperty "Saved queries reproduce state" $ withMaxSuccess 1 savedQueriesReproduceState
  , QC.testProperty "Delete non-existent animal leaves state unchanged" $ withMaxSuccess 1 deleteNonExistent
  , QC.testProperty "Statements -> String -> State round-trip" $ withMaxSuccess 1 statementsToStringToState
  , QC.testProperty "Statements -> State -> Statements round-trip" $ withMaxSuccess 1 statementsToStateToStatements
  ]


-- Statements -> states -> statements
saveThenLoad :: Property -- defines a property test
saveThenLoad = forAll genStatements $ \statements ->
  let origState = Lib3.unmarshallState statements
      marshalledStatements = Lib3.marshallState origState
      restoredState = Lib3.unmarshallState marshalledStatements
  in extractAnimals origState == extractAnimals restoredState


savedQueriesReproduceState :: Property
savedQueriesReproduceState = forAll genStatements $ \statements ->
  let origState = Lib3.unmarshallState statements
      savedQueries = Lib3.renderStatements (Lib3.marshallState origState)
      parsedStatements = Lib3.parseStatements savedQueries
      restoredState = case parsedStatements of
          Right (st, _) -> Lib3.unmarshallState st
          Left _           -> Lib2.emptyState
  in extractAnimals origState == extractAnimals restoredState


deleteNonExistent:: Property
deleteNonExistent = forAll genAnimal $ \animal ->
  let deleteStatement = Lib3.Single (Lib2.Delete animal) -- initialState = Lib2.emptyState
      unmarshalledState = Lib3.unmarshallState deleteStatement
  in extractAnimals unmarshalledState == []


-- statements -> renderStatements -> parseStatements -> unmarsh to state
statementsToStringToState :: Property
statementsToStringToState = forAll genStatements $ \statements ->
  let origState = Lib3.unmarshallState statements
      renderedString = Lib3.renderStatements statements
      parsedStatements = Lib3.parseStatements renderedString
      restoredState = case parsedStatements of
          Right (stmts, _) -> Lib3.unmarshallState stmts
          Left _           -> Lib2.emptyState
  in extractAnimals origState == extractAnimals restoredState


-- Statements -> State -> Statements -> State
statementsToStateToStatements :: Property
statementsToStateToStatements = forAll genStatements $ \statements ->    
  let modifiedState = Lib3.unmarshallState statements -- initialState = Lib2.emptyState
      restoredStatements = Lib3.marshallState modifiedState
      finalState = Lib3.unmarshallState restoredStatements
  in extractAnimals modifiedState == extractAnimals finalState



-- GENERATORS :Generate random animals
genAnimal :: Gen Lib2.Animal
genAnimal = do
  species <- elements ["dog", "cat", "hamster", "fish", "bird"]
  name <- elements ["Max", "Tom", "Cypsius", "Bliur", "Skraidukas"]
  age <- choose (1, 15)
  return $ Lib2.Animal species name age

-- Generate random queries
genQuery :: Gen Lib2.Query
genQuery = oneof
  [ Lib2.Add <$> genAnimal
  , Lib2.Delete <$> genAnimal
  , return Lib2.ListAnimals
  , Lib2.CompoundQuery <$> genQuery <*> genQuery
  ]

-- Generate Statements (single or batch)
genStatements :: Gen Lib3.Statements
genStatements = oneof
  [ Lib3.Single <$> genQuery
  , Lib3.Batch <$> listOf1 genQuery
  ]
-- listOf1 ensures that generated list contains at least 1 el.

-- Helper Functions
extractAnimals :: Lib2.State -> [Lib2.Animal]
extractAnimals (Lib2.State animals) = animals
