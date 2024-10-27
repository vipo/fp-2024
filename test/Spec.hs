{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Lib1 qualified
import Lib2 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup 
  "Lib2 tests"
  [ 
    testCase "Parse Nonsense" $
        Lib2.parseQuery "Nonsense"
          @?= Left "Failed to parse: Unknown command",
    testCase "Parse AddStorage Query" $
      Lib2.parseQuery "add_storage Beverage Soda 10" @?=
        Right (Lib2.AddStorage (Lib2.Storage [Lib2.Beverage "Soda" 10])),

    testCase "Parse SellItem Query" $
      Lib2.parseQuery "sell Apples 5" @?=
        Right (Lib2.SellItem "Apples" 5),

    testCase "Parse RestockItems Query" $
      Lib2.parseQuery "restock Apples 5" @?=
        Right (Lib2.RestockItems "Apples" 5),

    testCase "Parse RemoveItem Query" $
      Lib2.parseQuery "remove_item Apples" @?=
        Right (Lib2.RemoveItem "Apples"),

    testCase "Parse ShowInventory Query" $
      Lib2.parseQuery "show_inventory" @?=
        Right Lib2.ShowInventory,

    testCase "State Transition - AddStorage" $
      let initialState = Lib2.emptyState
          query = Lib2.AddStorage (Lib2.Storage [Lib2.Food Lib2.Fruits "Apples" 10])
          expectedState = initialState { Lib2.inventory = [Lib2.ItemWithId 1 (Lib2.Food Lib2.Fruits "Apples" 10)], Lib2.nextId = 2 }
      in Lib2.stateTransition initialState query @?= Right (Just "Added to inventory: [Food Fruits \"Apples\" 10]", expectedState),

    testCase "State Transition - SellItem" $
      let initialState = Lib2.State { Lib2.inventory = [Lib2.ItemWithId 1 (Lib2.Food Lib2.Fruits "Apples" 10)], Lib2.nextId = 2 }
          query = Lib2.SellItem "Apples" 5
          expectedState = Lib2.State { Lib2.inventory = [Lib2.ItemWithId 1 (Lib2.Food Lib2.Fruits "Apples" 5)], Lib2.nextId = 2 }
      in Lib2.stateTransition initialState query @?= Right (Just "Sold item: Apples with quantity 5", expectedState),

    testCase "State Transition - RestockItems" $
      let initialState = Lib2.State { Lib2.inventory = [Lib2.ItemWithId 1 (Lib2.Food Lib2.Fruits "Apples" 5)], Lib2.nextId = 2 }
          query = Lib2.RestockItems "Apples" 10
          expectedState = Lib2.State { Lib2.inventory = [Lib2.ItemWithId 1 (Lib2.Food Lib2.Fruits "Apples" 15)], Lib2.nextId = 2 }
      in Lib2.stateTransition initialState query @?= Right (Just "Restocked item: Apples with quantity 10", expectedState),

    testCase "State Transition - RemoveItem" $
      let initialState = Lib2.State { Lib2.inventory = [Lib2.ItemWithId 1 (Lib2.Food Lib2.Fruits "Apples" 5)], Lib2.nextId = 2 }
          query = Lib2.RemoveItem "Apples"
          expectedState = Lib2.State { Lib2.inventory = [], Lib2.nextId = 2 }
      in Lib2.stateTransition initialState query @?= Right (Just "Removed item with name: Apples", expectedState),

    testCase "State Transition - ShowInventory" $
      let initialState = Lib2.State { Lib2.inventory = [Lib2.ItemWithId 1 (Lib2.Food Lib2.Fruits "Apples" 5)], Lib2.nextId = 2 }
      in Lib2.stateTransition initialState Lib2.ShowInventory @?= Right (Just "Inventory list:\nItem ID: 1, Item: Food Fruits \"Apples\" 5\n", initialState)
  ]