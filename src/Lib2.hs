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
      emptyState
      --stateTransition,
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
parseDigit [] = Left "Cannot find any digits in an empty input"
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
        Left err -> Left ("Failed to parse initial product in products list: " ++ err)
        Right (prod, rest) -> parseMoreProducts [prod] rest
  where
    parseMoreProducts :: [Product] -> String -> Either String ([Product], String)
    parseMoreProducts acc remainingInput =
        case parseString ", " remainingInput of
            Left _ -> Right (acc, trim remainingInput)  -- No more products, end the list
            Right (_, restAfterComma) -> 
                case parseProduct (dropWhile C.isSpace restAfterComma) of
                    Left err -> Left ("Failed to parse product after comma in products list: " ++ err)
                    Right (nextProd, rest) -> parseMoreProducts (acc ++ [nextProd]) rest

-- Helper function to remove leading and trailing whitespace
trim :: String -> String
trim = L.dropWhileEnd C.isSpace . dropWhile C.isSpace


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

-- <add_command> ::= "add " <products>
parseAddCommand :: Parser Query
parseAddCommand = and2' (\_ ps -> AddCommand ps) 
                  (parseString "add ")
                  parseProducts


-- Helper parser to wrap a single Product into Products
parseSingleProductAsProducts :: Parser Products
parseSingleProductAsProducts input =
    case parseProduct input of
        Left err -> Left err
        Right (p, rest) -> Right (Products [p], rest)


-- <discount_command> ::= "giveDiscount " <product> " " <discount>
parseGiveDiscountCommand :: Parser Query
parseGiveDiscountCommand = and4' (\_ product _ discount -> GiveDiscountCommand product discount)
                          (parseString "giveDiscount ")
                          parseProduct
                          (parseChar ' ') 
                          parseDiscount

-- <buy_command> ::= "buy " <quantity> " " <product>
parseBuyCommand :: Parser Query
parseBuyCommand = and4' (\_ quantity _ product -> BuyCommand quantity product)
                          (parseString "buy ")
                          parseQuantity
                          (parseChar ' ') 
                          parseProduct

-- <compare_command> ::= "compare " <product> " " <product>
parseCompareCommand :: Parser Query
parseCompareCommand = and4' (\_ product1 _ product2 -> CompareCommand product1 product2)
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
parseQuery :: String -> Either String Query
parseQuery s =
    case orX
        [ parseRoundCommand
        , parseCheckShippingCommand
        , parseAddCommand
        , parseGiveDiscountCommand
        , parseBuyCommand
        , parseCompareCommand
        ] s of
            Left e -> Left e
            Right (q, _) -> Right q                      

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
    { products = []
    , discounts = []
    , purchaseHistory = []
    }


