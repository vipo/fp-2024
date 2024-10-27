{-# LANGUAGE InstanceSigs #-}
module Lib2
    ( Query(..),
      Product(..),
      Products(..),
      parseQuery,
      parseProduct,
      parseProducts,
      parseComponent,
      parseRoundCommand,
      parseAddOnName,
      parseBoardGameName,
      parseComponentName,
      parseQuantity,
      parseDiscount,
      parsePrice,
      parseBoardGame,
      parseBoardGameWithAddOns,
      parseAddOn,
      State(..),
      emptyState,
      stateTransition
    ) where

import qualified Data.Char as C
import qualified Data.List as L

type Parser a = String -> Either String (a, String)

data Products = Products [Product] deriving (Eq, Show) 
data Product = BoardGame String Double [Product]
             | AddOn String Double
             | Component Integer String
             | BoardGameWithAddOns String Double [Product] [Product]
             deriving (Eq, Show)

-- OR & ANDs
and2' :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
and2' c a b = \input ->
    case a input of
        Right (v1, r1) ->
            case b r1 of
                Right (v2, r2) -> Right (c v1 v2, r2)
                Left e2 -> Left e2
        Left e1 -> Left e1  
        
and3' :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
and3' f a b c = \input ->
    case a input of
        Right (v1, r1) ->
            case b r1 of
                Right (v2, r2) ->
                    case c r2 of
                        Right (v3, r3) -> Right (f v1 v2 v3, r3)
                        Left e3 -> Left e3
                Left e2 -> Left e2
        Left e1 -> Left e1  

and4' :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
and4' e a b c d = \input ->
    case a input of
        Right (v1, r1) ->
            case b r1 of
                Right (v2, r2) ->
                    case c r2 of
                        Right (v3, r3) ->
                            case d r3 of
                                Right (v4, r4) -> Right (e v1 v2 v3 v4, r4)
                                Left e4 -> Left e4
                        Left e3 -> Left e3
                Left e2 -> Left e2
        Left e1 -> Left e1  
{-
and5' :: (a -> b -> c -> d -> e -> f) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f
and5' f a b c d e = \input ->
    case a input of
        Right (v1, r1) ->
            case b r1 of
                Right (v2, r2) ->
                    case c r2 of
                        Right (v3, r3) ->
                            case d r3 of
                                Right (v4, r4) ->
                                    case e r4 of
                                        Right (v5, r5) -> Right (f v1 v2 v3 v4 v5, r5)
                                        Left e5 -> Left e5
                                Left e4 -> Left e4
                        Left e3 -> Left e3
                Left e2 -> Left e2
        Left e1 -> Left e1  
-}
and6' :: (a -> b -> c -> d -> e -> f -> g) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g
and6' g a b c d e f = \input ->
    case a input of
        Right (v1, r1) ->
            case b r1 of
                Right (v2, r2) ->
                    case c r2 of
                        Right (v3, r3) ->
                            case d r3 of
                                Right (v4, r4) ->
                                    case e r4 of
                                        Right (v5, r5) ->
                                             case f r5 of
                                            Right (v6, r6) -> Right (g v1 v2 v3 v4 v5 v6, r6)
                                            Left e6 -> Left e6
                                        Left e5 -> Left e5
                                Left e4 -> Left e4
                        Left e3 -> Left e3
                Left e2 -> Left e2
        Left e1 -> Left e1  

orX :: [Parser a] -> Parser a
orX [] _ = Left "No parser matched"
orX (p : ps) s = case p s of
  Left _ -> orX ps s
  Right res -> Right res


parseDigit :: Parser Char
-- parseDigit [] = Left "Cannot find any digits in an empty input"
parseDigit s@(h:t) = if C.isDigit h 
                      then Right (h, t) 
                      else Left ("'" ++ s ++ "'" ++ " does not start with a digit")

parseNumber :: Parser Integer
parseNumber [] = Left "Empty input, cannot parse a number"
parseNumber str =
    let
        digits = L.takeWhile C.isDigit str
        rest = drop (length digits) str
    in
        case digits of
            [] -> Left "Not a number"
            _ -> Right (read digits, rest)

parseChar :: Char -> Parser Char
parseChar c [] = Left ("Cannot find " ++ "'" ++ [c] ++ "'")
parseChar c s@(h:t) = if c == h 
                        then Right (c, t) 
                        else Left ("'" ++ [c] ++ "'" ++ " is not found" )

parseString :: String -> Parser String
parseString str [] = Left ("Cannot find " ++ str ++ " in an empty input")
parseString str input = if L.isPrefixOf str input 
                            then Right (str, drop (length str) input)
                            else Left ("'" ++ str ++ "'" ++ " is not found")

-- BNF implementation

-- <quantity> ::= <number>
parseQuantity :: Parser Integer
parseQuantity = parseNumber

-- <discount> ::= <number> "%"
parseDiscount :: Parser Integer
parseDiscount = and2' (\number _ -> number) 
                parseNumber 
                (parseChar '%')

-- <price> ::= <number> "eur" | <number> "." <number> "eur"
parsePrice :: Parser Double
parsePrice = orX 
    [ and2' (\num _ -> fromIntegral num) parseNumber (parseString "eur")
    , and4' (\num1 _ num2 _ -> read (show num1 ++ "." ++ show num2)) 
            parseNumber 
            (parseChar '.') 
            parseNumber 
            (parseString "eur")
    ]

-- <boardgame_name> ::= "corporateCEOTM" | "baseTM" | ...
parseBoardGameName :: Parser String
parseBoardGameName = orX 
    [ parseString "corporateCEOTM"
    , parseString "baseTM"
    , parseString "bigBoxTM"
    , parseString "venusTMexp"
    , parseString "turmoilTMexp"
    , parseString "preludeTMexp"
    , parseString "prelude1TMexp"
    , parseString "prelude2TMexp"
    , parseString "coloniesTMexp"
    , parseString "ellas&hellasTMexp"
    , parseString "automaTMexp"
    , parseString "baseTMAE"
    , parseString "discoveryTMAEexp"
    , parseString "foundationsTMAEexp"
    , parseString "crisisTMAEexp"
    ]

-- <boardgame> ::= <boardgame_name> " " <price> " (contains: " <products> ")"
parseBoardGame :: Parser Product
parseBoardGame = and6' (\name _ price _ components _ -> BoardGame name price components)
                    parseBoardGameName
                    (parseChar ' ')
                    parsePrice
                    (parseString " (contains: ")
                    parseProducts
                    (parseChar ')')

-- <boardgame_with_addons> ::= <boardgame> "[includes: " <products> "]"
parseBoardGameWithAddOns :: Parser Product
parseBoardGameWithAddOns = and4' (\(BoardGame name price components) _ addons _ -> 
                                  BoardGameWithAddOns name price components addons)
                                  parseBoardGame
                                  (parseString " [includes: ")
                                  parseProducts
                                  (parseChar ']')

-- <products> ::= <product> | <product> ", " <products>
parseProducts :: Parser [Product]
parseProducts input = 
  case parseProduct input of
    Right (p, remaining) -> 
      case parseString ", " remaining of
        Right (_, restAfterComma) ->
          case parseProducts restAfterComma of
            Right (moreProducts, finalRest) -> 
              Right (p : moreProducts, finalRest)
            Left _ -> Right ([p], remaining) -- No more products after the comma
        Left _ -> Right ([p], remaining) -- No comma, single product list
    Left _ -> Right ([], input)

-- <product> ::= <boardgame_with_addons> | <boardgame> | <add_on> | <component>
parseProduct :: Parser Product
parseProduct = orX 
    [ parseBoardGameWithAddOns
    , parseBoardGame
    , parseAddOn
    , parseComponent
    ]

-- <component_name> ::= "tile" | "gameBoard" | ...
parseComponentName :: Parser String
parseComponentName = orX 
    [ parseString "tile"
    , parseString "gameBoard"
    , parseString "playerBoard"
    , parseString "card"
    , parseString "marker"
    , parseString "rules"
    ]

-- <component> ::= <quantity> " " <component_name>
parseComponent :: Parser Product
parseComponent =
    and3' (\quantity _ name-> Component quantity name) 
    parseQuantity 
    (parseChar ' ') 
    parseComponentName 

-- <add_on_name> ::= "playerBoard" | "miniature" | ...
parseAddOnName :: Parser String
parseAddOnName = orX 
    [ parseString "playerBoard"
    , parseString "miniature"
    , parseString "metalResource"
    , parseString "cardSleeve"
    , parseString "spaceInsert"
    ]

-- <add_on> ::= <add_on_name> " " <price> "eur"
parseAddOn :: Parser Product
parseAddOn = and3' (\name _ price -> AddOn name price)
                parseAddOnName
                (parseChar ' ')
                parsePrice


-- | An entity which represets user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.
-- The Query type representing user commands
data Query = RoundCommand Product
           | CheckShippingCommand Product
           | AddCommand [Product] 
           | GiveDiscountCommand Product Integer  
           | BuyCommand Integer Product
           | CompareCommand Product Product
           | ViewCommand

-- <view_command> ::= "view"
parseViewCommand :: Parser Query
parseViewCommand input = case parseString "view" input of
    Right (_, rest) -> Right (ViewCommand, rest)
    Left err -> Left err

-- <round_command> ::= "roundTo " <product>
parseRoundCommand :: Parser Query
parseRoundCommand = and2' (\_ p -> RoundCommand p)
                          (parseString "roundTo ")
                          parseProduct

-- <check_shipping_command> ::= "checkShipping " <product>
parseCheckShippingCommand :: Parser Query
parseCheckShippingCommand = and2' (\_ p -> CheckShippingCommand p)
                          (parseString "checkShipping ")
                          parseProduct

-- <add_command> ::= "add " <products>
parseAddCommand :: Parser Query
parseAddCommand = and2' (\_ ps -> AddCommand ps) 
                  (parseString "add ")
                  parseProducts

-- <discount_command> ::= "giveDiscount " <product> " " <discount>
parseGiveDiscountCommand :: Parser Query
parseGiveDiscountCommand = and4' (\_ p _ discount -> GiveDiscountCommand p discount)
                          (parseString "giveDiscount ")
                          parseProduct
                          (parseChar ' ') 
                          parseDiscount

-- <buy_command> ::= "buy " <quantity> " " <product>
parseBuyCommand :: Parser Query
parseBuyCommand = and4' (\_ quantity _ p -> BuyCommand quantity p)
                          (parseString "buy ")
                          parseQuantity
                          (parseChar ' ') 
                          parseProduct

-- <compare_command> ::= "compare " <product> " " <product>
parseCompareCommand :: Parser Query
parseCompareCommand = and4' (\_ p1 _ p2 -> CompareCommand p1 p2)
                          (parseString "compare ")
                          parseProduct
                          (parseChar ' ') 
                          parseProduct

                          
-- | The instances are needed basically for tests
--instance Eq Query where
--  (==) _ _= False
instance Eq Query where
    (RoundCommand p1) == (RoundCommand p2) = p1 == p2
    (CheckShippingCommand p1) == (CheckShippingCommand p2) = p1 == p2
    (AddCommand ps1) == (AddCommand ps2) = ps1 == ps2
    (GiveDiscountCommand p1 d1) == (GiveDiscountCommand p2 d2) = p1 == p2 && d1 == d2
    (BuyCommand q1 p1) == (BuyCommand q2 p2) = q1 == q2 && p1 == p2
    (CompareCommand p1 q1) == (CompareCommand p2 q2) = p1 == p2 && q1 == q2
    _ == _ = False

--instance Show Query where
--  show _ = ""
instance Show Query where
    show (RoundCommand p) = "RoundCommand " ++ show p
    show (CheckShippingCommand p) = "CheckShippingCommand " ++ show p
    show (AddCommand ps) = "AddCommand " ++ show ps
    show (GiveDiscountCommand p d) = "GiveDiscountCommand " ++ show p ++ " " ++ show d
    show (BuyCommand q p) = "BuyCommand " ++ show q ++ " " ++ show p
    show (CompareCommand p1 p2) = "CompareCommand " ++ show p1 ++ " " ++ show p2


-- | Parses user's input.
-- The function must have tests.
-- parseQuery :: String -> Either String Query
-- parseQuery _ = Left "Not implemented 2"
{-
parseQuery :: String -> Either String Query
parseQuery s =
    case orX
        [ parseRoundCommand
        , parseCheckShippingCommand
        , parseAddCommand
        , parseGiveDiscountCommand
        , parseBuyCommand
        , parseCompareCommand
        , parseViewCommand
        ] s of
            Left e -> Left e
            Right (q, _) -> Right q
-}         

type PurchaseHistory = [(Product, Integer)]

-- | An entity which represents your program's state.
-- Currently it has no constructors but you can introduce
-- as many as needed.
-- Update your State data type to include purchase history
data State = State
    { products :: [Product]
    , discounts :: [(Product, Integer)]
    , purchaseHistory :: PurchaseHistory
    } deriving (Eq, Show)

-- | Creates an initial program's state.
-- It is called once when the program starts.
emptyState :: State
emptyState = State 
    { products = [],
      discounts = [],
      purchaseHistory = []
    }


viewState :: State -> String
viewState (State products discounts purchaseHistory) =
  "Current State:\n"
    ++ "Products:\n"
    ++ unlines (map show products)
    ++ "Discounts:\n"
    ++ unlines (map (\(p, d) -> show p ++ " with " ++ show d ++ "% discount") discounts)
    ++ "Purchase History:\n"
    ++ unlines (map (\(p, q) -> show q ++ " units of " ++ show p) purchaseHistory)


-- | Updates a state according to a query. This allows your program to share the state between repl iterations.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition state query = case query of
  AddCommand newProducts ->
    let updatedProducts = products state ++ newProducts
        newState = state { products = updatedProducts }
    in Right (Just "New products added to the state.", newState)

  GiveDiscountCommand product discount ->
    let updatedProducts = map (\p -> if p == product then product else p) (products state)
        newState = state { products = updatedProducts }
    in Right (Just "Discount applied to the product.", newState)

  BuyCommand quantity product ->
    let newPurchaseHistory = (product, quantity) : purchaseHistory state
        newState = state { purchaseHistory = newPurchaseHistory }
    in Right (Just "Product bought and added to purchase history.", newState)

  RoundCommand product ->
    Right (Just "Product price rounded.", state)

  CheckShippingCommand product ->
    Right (Just "Shipping cost calculated for the product.", state)

  CompareCommand product1 product2 ->
    Right (Just "Comparison between products done.", state)

  ViewCommand ->
    Right (Just $ "State: " ++ viewState state, state)


parseQuery :: String -> Either String Query
parseQuery input = 
  case parseAddCommand input of
    Right (query, _) -> Right query
    Left _ -> case parseRoundCommand input of
      Right (query, _) -> Right query
      Left _ -> case parseCheckShippingCommand input of
        Right (query, _) -> Right query
        Left _ -> case parseGiveDiscountCommand input of
          Right (query, _) -> Right query
          Left _ -> case parseBuyCommand input of
            Right (query, _) -> Right query
            Left _ -> case parseCompareCommand input of
              Right (query, _) -> Right query
              Left _ -> case parseViewCommand input of
                Right (query, _) -> Right query
                Left _ -> Left "Error: command doesn't match anything from query."

