{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=), assertFailure )
import Lib2 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Lib2 tests"
  [ 
    testCase "Parsing empty query" $
     Lib2.parseQuery "" @?= Left "Unknown command",

    testGroup "parseBoardGame tests"
      [ testCase "Parsing a valid board game with components" $
          Lib2.parseBoardGame "corporateCEOTM 100eur (contains: 2 tile, 1 gameBoard)" 
          @?= Right (Lib2.BoardGame "corporateCEOTM" 100.0 [Lib2.Component 2 "tile", Lib2.Component 1 "gameBoard"], ""),
        
        testCase "Parsing a board game with decimal price and components" $
          Lib2.parseBoardGame "baseTM 99.99eur (contains: 3 card, 5 marker)" 
          @?= Right (Lib2.BoardGame "baseTM" 99.99 [Lib2.Component 3 "card", Lib2.Component 5 "marker"], ""),

        testCase "Parsing a board game with missing closing parenthesis" $
          Lib2.parseBoardGame "venusTMexp 150eur (contains: 4 tile, 1 playerBoard" 
          @?= Left "Cannot find ')'",

        testCase "Parsing a board game with missing 'contains:'" $
          Lib2.parseBoardGame "turmoilTMexp 180eur ()" 
          @?= Left "' (contains: ' is not found"
      ],

    testGroup "parseComponent tests"
      [ testCase "Parsing a valid component with quantity and name" $
          Lib2.parseComponent "2 tile" @?= Right (Lib2.Component 2 "tile", ""),
        
        testCase "Parsing a valid component with different component name" $
          Lib2.parseComponent "3 gameBoard" @?= Right (Lib2.Component 3 "gameBoard", ""),
        
        testCase "Parsing a valid component with single quantity and name" $
          Lib2.parseComponent "1 card" @?= Right (Lib2.Component 1 "card", ""),
        
        testCase "Parsing an invalid component with non-numeric quantity" $
          Lib2.parseComponent "six marker" @?= Left "Not a number",
        
        testCase "Parsing an invalid component with missing space" $
          Lib2.parseComponent "5card" @?= Left "' ' is not found",
        
        testCase "Parsing an invalid component with unrecognized component name" $
          Lib2.parseComponent "2 gdhjasdhga" @?= Left "No parser matched",

        testCase "Parsing an incomplete component with only quantity" $
          Lib2.parseComponent "7 " @?= Left "No parser matched"
      ],

    testGroup "parseAddOn tests"
      [ testCase "Parsing valid add-on with name 'cardSleeve' and price 5eur" $
          Lib2.parseAddOn "cardSleeve 5eur"
          @?= Right (Lib2.AddOn "cardSleeve" 5.0, ""),

        testCase "Parsing valid add-on with name 'metalResource' and price 15eur" $
          Lib2.parseAddOn "metalResource 15eur"
          @?= Right (Lib2.AddOn "metalResource" 15.0, ""),

        testCase "Parsing valid add-on with name 'spaceInsert' and price 7.50eur" $
          Lib2.parseAddOn "spaceInsert 7.50eur"
          @?= Right (Lib2.AddOn "spaceInsert" 7.50, "")
      ],

    testGroup "Basic Product Parsing Tests"
      [ testCase "Parsing a board game with components" $
          Lib2.parseProduct "corporateCEOTM 100eur (contains: 2 tile, 1 gameBoard)"
          @?= Right (Lib2.BoardGame "corporateCEOTM" 100.0 [Lib2.Component 2 "tile", Lib2.Component 1 "gameBoard"], ""),

        testCase "Parsing an add-on" $
          Lib2.parseProduct "cardSleeve 5eur"
          @?= Right (Lib2.AddOn "cardSleeve" 5.0, ""),

        testCase "Parsing a component" $
          Lib2.parseProduct "3 marker"
          @?= Right (Lib2.Component 3 "marker", "")
      ],

    testGroup "Advanced Product Parsing with Add-Ons"
      [ testCase "Parsing a board game with add-ons" $
          Lib2.parseProduct 
          "corporateCEOTM 100eur (contains: 2 tile, 1 gameBoard) [includes: cardSleeve 5eur, miniature 10eur]"
          @?= Right (Lib2.BoardGameWithAddOns "corporateCEOTM" 100.0 
                [Lib2.Component 2 "tile", Lib2.Component 1 "gameBoard"]
                [Lib2.AddOn "cardSleeve" 5.0, Lib2.AddOn "miniature" 10.0], ""),

        testCase "Parsing a board game with multiple add-ons" $
          Lib2.parseProduct 
          "baseTM 80eur (contains: 3 card, 5 marker) [includes: playerBoard 15eur, metalResource 20eur]"
          @?= Right (Lib2.BoardGameWithAddOns "baseTM" 80.0 
                [Lib2.Component 3 "card", Lib2.Component 5 "marker"]
                [Lib2.AddOn "playerBoard" 15.0, Lib2.AddOn "metalResource" 20.0], "")
      ],

    testGroup "Other Cases"
      [ testCase "Parsing an invalid product format" $
          Lib2.parseProduct "invalidProduct 100" 
          @?= Left "No parser matched",

        testCase "Parsing a board game missing [includes: ...]" $
          Lib2.parseProduct "corporateCEOTM 100eur (contains: 2 tile, 1 gameBoard)" 
          @?= Right (Lib2.BoardGame "corporateCEOTM" 100.0 [Lib2.Component 2 "tile", Lib2.Component 1 "gameBoard"], "")
      ],

      testCase "Parsing a complex board game with add-ons and components" $
          Lib2.parseProduct "bigBoxTM 150eur (contains: 2 tile, 1 gameBoard, 5 marker) [includes: playerBoard 10eur, metalResource 20eur]" 
          @?= Right (Lib2.BoardGameWithAddOns "bigBoxTM" 150.0
                [Lib2.Component 2 "tile", Lib2.Component 1 "gameBoard", Lib2.Component 5 "marker"]
                [Lib2.AddOn "playerBoard" 10.0, Lib2.AddOn "metalResource" 20.0], "")

  ]
