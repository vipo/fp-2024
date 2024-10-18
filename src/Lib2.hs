{-# LANGUAGE InstanceSigs #-}
module Lib2
    ( Query(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition
    ) where

import qualified Data.Char as C
import qualified Data.List as L

type Parser a = String -> Either String (a, String)

data Products = Products [Product] deriving (Eq, Show) 
data Product = BoardGame String Double Components
             | AddOn String Double
             | AddOns Products
             | BoardGame' String Double Components Products
             deriving (Eq, Show)
            
data Components = Components [Component] deriving (Eq, Show)
data Component = Component Integer String deriving (Eq, Show)

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

or2 :: Parser a -> Parser a -> Parser a
or2 a b = \input ->
    case a input of
        Right r1 -> Right r1
        Left e1 ->
            case b input of
                Right r2 -> Right r2
                Left e2 -> Left ("Error while parsing first: " ++ e1 ++ "and " ++ "second: " ++ e2 ++ " values")

parseDigit :: Parser Char
parseDigit [] = Left "Cannot find any digits in an empty input"
parseDigit s@(h:t) = if C.isDigit h 
                      then Right (h, t) 
                      else Left (s ++ " does not start with a digit")

parseNumber :: Parser Integer
parseNumber [] = Left "empty input, cannot parse a number"
parseNumber str =
    let
        digits = L.takeWhile C.isDigit str
        rest = drop (length digits) str
    in
        case digits of
            [] -> Left "not a number"
            _ -> Right (read digits, rest)

parseChar :: Char -> Parser Char
parseChar c [] = Left ("Cannot find " ++ [c] ++ " in an empty input")
parseChar c s@(h:t) = if c == h 
                        then Right (c, t) 
                        else Left (c : " is not found in " ++ s)

parseString :: String -> Parser String
parseString str [] = Left ("Cannot find " ++ str ++ " in an empty input")
parseString str input = if L.isPrefixOf str input 
                            then Right (str, drop (length str) input)
                            else Left (str ++ " is not found in " ++ input)

-- <quantity> ::= <number>
parseQuantity :: Parser Integer
parseQuantity = parseNumber

-- <discount> ::= <number> "%"
parseDiscount :: Parser Integer
parseDiscount = and2' (\number _ -> number) parseNumber (parseChar '%')

-- <price> ::= <number> "eur" | <number> "." <number> "eur"
parsePrice :: Parser Double
parsePrice = 
    and2' (\num _ -> fromIntegral num) parseNumber (parseString "eur") 
    `or2` 
    and4' (\num1 _ num2 _ -> read (show num1 ++ "." ++ show num2)) 
           parseNumber 
           (parseChar '.') 
           parseNumber 
           (parseString "eur")
          
-- <boardgame_name> ::= "corporateCEOTM" | "baseTM" | ...
parseBoardGameName :: Parser String
parseBoardGameName = 
    parseString "corporateCEOTM" 
    `or2` parseString "baseTM" 
    `or2` parseString "bigBoxTM" 
    `or2` parseString "venusTMexp" 
    `or2` parseString "turmoilTMexp" 
    `or2` parseString "preludeTMexp" 
    `or2` parseString "prelude1TMexp" 
    `or2` parseString "prelude2TMexp" 
    `or2` parseString "coloniesTMexp" 
    `or2` parseString "ellas&hellasTMexp" 
    `or2` parseString "automaTMexp" 
    `or2` parseString "baseTMAE" 
    `or2` parseString "discoveryTMAEexp" 
    `or2` parseString "foundationsTMAEexp" 
    `or2` parseString "crisisTMAEexp" 

-- <boardgame> ::= <boardgame_name> " " <price> " (contains: " <components> ")"
parseBoardGame :: Parser Product
parseBoardGame = and5' (\name price _ components _ -> BoardGame name price components) 
                    parseBoardGameName 
                    parsePrice 
                    (parseString " (contains: ") 
                    parseComponents 
                    (parseChar ')')

-- <products> ::= <product> | <product> ", " <products>
parseProducts :: Parser Products
parseProducts [] = Left ("Product/ts input is empty")
parseProducts input = 
    case parseProduct input of
        Right (prod, rest) -> Right (Products [prod], rest) 
        Left _ -> case parseProduct input of
            Left _ -> Left ("Products is empty")
            Right (product, rest) -> 
                case parseString ", " rest of
                    Right (_, rest2) -> 
                        case parseProducts rest2 of
                            Right (Products ps, finalRest) -> 
                                Right (Products (product : ps), finalRest)
                            Left err -> Left ("Error from recursive parsing")
                    Left err -> Left ("Error while parsing \", \"")  

-- <product> ::= <boardgame> | <add_on> | <add_ons> | <boardgame> "[includes: " <products> "]"
parseProduct :: Parser Product
parseProduct = 
    parseBoardGame 
    `or2`
    parseAddOn 
    `or2`
    parseAddOns 
    `or2`
    and4' (\boardGame _ products _ -> 
                case boardGame of
                    BoardGame name price components -> BoardGame' name price components products
                    _ -> error ("Unexpected parse failure")
          )
          parseBoardGame 
          (parseString " [includes: ") 
          parseProducts 
          (parseChar ']')

-- <component_name> ::= "tile" | "gameBoard" | ...
parseComponentName :: Parser String
parseComponentName = 
    parseString "tile" 
    `or2` parseString "gameBoard" 
    `or2` parseString "playerBoard" 
    `or2` parseString "card" 
    `or2` parseString "marker" 
    `or2` parseString "rules" 


-- <component> ::= <quantity> " " <component_name>
parseComponent :: Parser Component
parseComponent =
    and2' (\quantity name-> Component quantity name) 
    parseQuantity 
    parseComponentName 

-- <components> ::= <component> | <component> ", " <components> 
parseComponents :: Parser Components
parseComponents input = 
    case parseComponent input of
        Right (comp, rest) -> 
            Right (Components [comp], rest) 
        Left _ -> 
            case parseComponent input of
                Left _ -> Left "Components is empty"
                Right (component, rest) -> 
                    case parseString ", " rest of
                        Right (_, rest2) -> 
                            case parseComponents rest2 of
                                Right (Components ps, finalRest) -> 
                                    Right (Components (component : ps), finalRest)
                                Left err -> Left ("Error from recursive parsing")
                        Left err -> Left ("Error while parsing \", \"")

-- <add_on_name> ::= "playerBoard" | "miniature" | ...
parseAddOnName :: Parser String
parseAddOnName = 
    parseString "playerBoard" 
    `or2` parseString "miniature" 
    `or2` parseString "metalResource" 
    `or2` parseString "cardSleeve" 
    `or2` parseString "spaceInsert" 

-- <add_on> ::= <add_on_name> " " <price> "eur"
parseAddOn :: Parser Product      
parseAddOn = 
    and4' (\name _ price _ -> AddOn name price) 
        parseAddOnName 
        (parseChar ' ') 
        parsePrice 
        (parseString "eur")

-- <add_ons> ::= <add_on> | <add_on> ", " <add_ons>
parseAddOns :: Parser Product
parseAddOns input = 
    case parseAddOn input of
        Right (addOn, rest) -> 
            Right (AddOns (Products [addOn]), rest)
        Left _ -> 
            case parseAddOn input of
                Left _ -> Left ("Add-ons input is empty")
                Right (addOn, rest) -> 
                    case parseString ", " rest of
                        Right (_, rest2) -> 
                            case parseAddOns rest2 of
                                Right (AddOns (Products addOns), finalRest) -> 
                                    Right (AddOns (Products (addOn : addOns)), finalRest)
                                Left err -> Left ("Error from recursive parse")
                        Left err -> Left ("Error while parsing \", \"")


-- | An entity which represets user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.
-- The Query type representing user commands
data Query = RoundCommand Product
           | CheckShippingCommand Product
           | AddCommand Product Product
           | GiveDiscountCommand Product Integer  
           | BuyCommand Integer Product
           | CompareCommand Product Product

-- <round_command> ::= "roundTo " <product>
parseRoundCommand :: Parser Query
parseRoundCommand = and2' (\_ product -> RoundCommand product)
                          (parseString "roundTo ")
                          parseProduct

-- <check_shipping_command> ::= "checkShipping " <product>
parseCheckShippingCommand :: Parser Query
parseCheckShippingCommand = and2' (\_ product -> CheckShippingCommand product)
                          (parseString "checkShipping ")
                          parseProduct

-- <add_command> ::= "add " <product> " " <product>
parseAddCommand :: Parser Query
parseAddCommand = and3' (\_ product1 product2 -> AddCommand product1 product2)
                          (parseString "add ")
                          parseProduct
                          parseProduct

-- <discount_command> ::= "giveDiscount " <product> " " <discount>
parseGiveDiscountCommand :: Parser Query
parseGiveDiscountCommand = and3' (\_ product discount -> GiveDiscountCommand product discount)
                          (parseString "giveDiscount ")
                          parseProduct
                          parseDiscount

-- <buy_command> ::= "buy " <quantity> " " <product>
parseBuyCommand :: Parser Query
parseBuyCommand = and3' (\_ quantity product -> BuyCommand quantity product)
                          (parseString "buy ")
                          parseQuantity
                          parseProduct

-- <compare_command> ::= "compare " <product> " " <product>
parseCompareCommand :: Parser Query
parseCompareCommand = and3' (\_ product1 product2-> CompareCommand product1 product2)
                          (parseString "compare ")
                          parseProduct
                          parseProduct
                          

-- | The instances are needed basically for tests
instance Eq Query where
  (==) _ _= False

instance Show Query where
  show _ = ""



or2' :: [Parser a] -> Parser a
or2' [] = \_ -> Left "All parsers failed"
or2' (p:ps) = \input -> case p input of
                         Right res -> Right res
                         Left _ -> or2' ps input

-- | Parses user's input.
-- The function must have tests.
-- parseQuery :: String -> Either String Query
-- parseQuery _ = Left "Not implemented 2"
parseQuery :: String -> Either String Query
parseQuery input = case or2' [ parseRoundCommand
                            , parseCheckShippingCommand
                            , parseAddCommand
                            , parseGiveDiscountCommand
                            , parseBuyCommand
                            , parseCompareCommand
                            ] input of
                      Right (query, "") -> Right query
                      Right (_, rest) -> Left ("Unparsed input: " ++ rest)
                      Left err -> Left err


-- | An entity which represents your program's state.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data State = State
    { products :: [Product]
    , discounts :: [(Product, Integer)]  -- List of products with discounts
    } deriving (Eq, Show)

-- | Creates an initial program's state.
-- It is called once when the program starts.
emptyState :: State
emptyState = State { products = [], discounts = [] }


-- | Creates an initial program's state.
-- It is called once when the program starts.
-- emptyState :: State
-- emptyState = error "Not implemented 1"

-- | Updates a state according to a query.
-- This allows your program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition _ _ = Left "Not implemented 3"

