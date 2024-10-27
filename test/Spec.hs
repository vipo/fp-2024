{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Lib2 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Lib2 tests"
  [ 

    testCase "Parsing empty query" $
     Lib2.parseQuery "" @?= Left "No parser matched",



  -- Testing base parsers
      testGroup "parseAddOnName tests"
      [ testCase "Parsing playerBoard" $
          Lib2.parseAddOnName "playerBoard" @?= Right ("playerBoard", ""),
        
        testCase "Parsing miniature" $
          Lib2.parseAddOnName "miniature" @?= Right ("miniature", ""),
        
        testCase "Parsing metalResource" $
          Lib2.parseAddOnName "metalResource" @?= Right ("metalResource", ""),
        
        testCase "Parsing cardSleeve" $
          Lib2.parseAddOnName "cardSleeve" @?= Right ("cardSleeve", ""),
        
        testCase "Parsing spaceInsert" $
          Lib2.parseAddOnName "spaceInsert" @?= Right ("spaceInsert", ""),
        
        testCase "Parsing invalid" $
          Lib2.parseAddOnName "asdrasfd" @?= Left "No parser matched"
      ],

      testGroup "parseComponentName tests"
      [ testCase "Parsing tile" $
          Lib2.parseComponentName "tile" @?= Right ("tile", ""),
        
        testCase "Parsing gameBoard" $
          Lib2.parseComponentName "gameBoard" @?= Right ("gameBoard", ""),
        
        testCase "Parsing playerBoard" $
          Lib2.parseComponentName "playerBoard" @?= Right ("playerBoard", ""),
        
        testCase "Parsing card" $
          Lib2.parseComponentName "card" @?= Right ("card", ""),
        
        testCase "Parsing marker" $
          Lib2.parseComponentName "marker" @?= Right ("marker", ""),
        
        testCase "Parsing rules" $
          Lib2.parseComponentName "rules" @?= Right ("rules", ""),
        
        testCase "Parsing invalid" $
          Lib2.parseComponentName "reutdrtdt" @?= Left "No parser matched"
      ],

    testGroup "parseBoardGameName tests"
      [ testCase "Parsing corporateCEOTM" $
          Lib2.parseBoardGameName "corporateCEOTM" @?= Right ("corporateCEOTM", ""),
        
        testCase "Parsing baseTM" $
          Lib2.parseBoardGameName "baseTM" @?= Right ("baseTM", ""),
        
        testCase "Parsing bigBoxTM" $
          Lib2.parseBoardGameName "bigBoxTM" @?= Right ("bigBoxTM", ""),
        
        testCase "Parsing venusTMexp" $
          Lib2.parseBoardGameName "venusTMexp" @?= Right ("venusTMexp", ""),
        
        testCase "Parsing turmoilTMexp" $
          Lib2.parseBoardGameName "turmoilTMexp" @?= Right ("turmoilTMexp", ""),
        
        testCase "Parsing preludeTMexp" $
          Lib2.parseBoardGameName "preludeTMexp" @?= Right ("preludeTMexp", ""),
        
        testCase "Parsing invalid" $
          Lib2.parseBoardGameName "ggsdhjdf" @?= Left "No parser matched"
      ],

    testGroup "parseQuantity tests"
      [ testCase "Parsing a valid quantity" $
          Lib2.parseQuantity "123" @?= Right (123, ""),
        
        testCase "Parsing an invalid quantity" $
          Lib2.parseQuantity "gfdgfd" @?= Left "Not a number"
      ],

    testGroup "parseDiscount tests"
      [ testCase "Parsing a valid discount with %" $
          Lib2.parseDiscount "20%" @?= Right (20, ""),
        
        testCase "Parsing an invalid discount without %" $
          Lib2.parseDiscount "20" @?= Left "Cannot find '%'",

        testCase "Parsing an invalid discount with non-numeric characters" $
          Lib2.parseDiscount "vghjv%" @?= Left "Not a number"
      ],

    testGroup "parsePrice tests"
      [ testCase "Parsing a valid integer price with eur" $
          Lib2.parsePrice "100eur" @?= Right (100.0, ""),
        
        testCase "Parsing a valid decimal price with eur" $
          Lib2.parsePrice "100.50eur" @?= Right (100.50, ""),
        
        testCase "Parsing an invalid price without eur" $
          Lib2.parsePrice "100" @?= Left "No parser matched",
        
        testCase "Parsing an invalid price with extra text" $
          Lib2.parsePrice "100eurABC" @?= Right (100.0, "ABC"),
        
        testCase "Parsing an invalid format with no number" $
          Lib2.parsePrice "eur" @?= Left "No parser matched"
      ],

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

    testGroup "Error Cases"
    [ testCase "Parsing an invalid product format" $
        Lib2.parseProduct "invalidProduct 100" 
        @?= Left "No parser matched",

      testCase "Parsing a board game missing [includes: ...]" $
        Lib2.parseProduct "corporateCEOTM 100eur (contains: 2 tile, 1 gameBoard)" 
        @?= Right (Lib2.BoardGame "corporateCEOTM" 100.0 [Lib2.Component 2 "tile", Lib2.Component 1 "gameBoard"], "")
    ]


  ]
