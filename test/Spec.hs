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
    QC.testProperty "Save-Then-Load preserves state" $ withMaxSuccess 1 prop_saveThenLoadPreservesState
  , QC.testProperty "Saved queries reproduce state" $ withMaxSuccess 1 prop_savedQueriesReproduceState
  , QC.testProperty "Delete non-existent animal leaves state unchanged" $ withMaxSuccess 1 prop_deleteNonExistentAnimalUnchanged
  ]

-- Property Test Implementations
-- Save-Then-Load Preserves State
prop_saveThenLoadPreservesState :: Property
prop_saveThenLoadPreservesState = forAll genStatements $ \statements ->
  let originalState = Lib3.unmarshallState statements
      marshalledStatements = Lib3.marshallState originalState
      restoredState = Lib3.unmarshallState marshalledStatements
  in extractAnimals originalState == extractAnimals restoredState

-- Saved Queries Reproduce State
prop_savedQueriesReproduceState :: Property
prop_savedQueriesReproduceState = forAll genStatements $ \statements ->
  let originalState = Lib3.unmarshallState statements
      savedQueries = Lib3.renderStatements (Lib3.marshallState originalState)
      parsedStatements = Lib3.parseStatements savedQueries
      restoredState = case parsedStatements of
          Right (stmts, _) -> Lib3.unmarshallState stmts
          Left _           -> Lib2.emptyState
  in extractAnimals originalState == extractAnimals restoredState

-- Delete Non-Existent Animal Leaves State Unchanged
prop_deleteNonExistentAnimalUnchanged :: Property
prop_deleteNonExistentAnimalUnchanged = forAll genAnimal $ \animal ->
  let initialState = Lib2.State []
      deleteStatement = Lib3.Single (Lib2.Delete animal)
      unmarshalledState = Lib3.unmarshallState deleteStatement
  in extractAnimals unmarshalledState == []


-- Generators
-- Generate random animals
genAnimal :: Gen Lib2.Animal
genAnimal = do
  species <- elements ["dog", "cat", "hamster", "fish", "bird"]
  name <- elements ["Max", "Whiskers", "Tom", "Luna"]
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

-- Generate Statements (either single or batch)
genStatements :: Gen Lib3.Statements
genStatements = oneof
  [ Lib3.Single <$> genQuery
  , Lib3.Batch <$> listOf1 genQuery
  ]

-- Helper Functions
extractAnimals :: Lib2.State -> [Lib2.Animal]
extractAnimals (Lib2.State animals) = animals
