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

data Command = RoundTo Product deriving Show

data Products = Products [Product] deriving Show

data Product = BoardGame String Double Components
             | BoardGame' String Double Components Products
             deriving Show
            
data Components = Components [Component] deriving Show
data Component = Component String Integer deriving Show

data AddOns = AddOns [AddOn] deriving Show
data AddOn = AddOn String Double deriving Show

-- <round_command> ::= "roundTo " <product>
parseRoundCommand :: Parser Command
parseRoundCommand = and2 RoundTo (parseString "roundTo ") parseProduct



-- <price> ::= <number> | <number> "." <number>
parsePrice :: Parser Double
parsePrice = fmap read (some parseDigit `or2` (some parseDigit <* parseChar '.' <*> some parseDigit))


and2 :: Parser a -> Parser b -> Parser (a, b)
and2 a b = \input ->
    case a input of
        Right (v1, r1) ->
            case b r1 of
                Right (v2, r2) -> Right ((v1, v2), r2)
                Left e2 -> Left ("Error while parsing second value: "  ++ e2)
        Left e1 -> Left ("Error while parsing first value: "  ++ e1)


or2 :: Parser a -> Parser a -> Parser a
or2 a b = \input ->
    case a input of
        Right r1 -> Right r1
        Left e1 ->
            case b input of
                Right r2 -> Right r2
                Left e2 -> Left ("Error while parsing first: " ++ e1 ++ "and " ++ "second: " ++ e2 ++ "values")


-- >>> parseDigit "432"
-- Right ('4',"32")
-- >>> parseDigit "labas"
-- Left "labas does not start with a digit"
parseDigit :: Parser Char
parseDigit [] = Left "Cannot find any digits in an empty input"
parseDigit s@(h:t) = if C.isDigit h 
                      then Right (h, t) 
                      else Left (s ++ " does not start with a digit")


-- >>> parseNumber "123d"
-- Right (123,"d")
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


-- >>> parseChar 'a' "aaa"
-- Right ('a',"aa")
-- >>> parseChar '*' "fdf"
-- Left "* is not found in fdf"
parseChar :: Char -> Parser Char
parseChar c [] = Left ("Cannot find " ++ [c] ++ " in an empty input")
parseChar c s@(h:t) = if c == h 
                        then Right (c, t) 
                        else Left (c : " is not found in " ++ s)


parseString :: String -> Parser String
parseString [] = Left "Input is empty"
parseString str input = if L.isPrefixOf str input 
                        then Right (str, drop (length str) input)
                        else Left (str ++ " is not found in " ++ input)


-- <boardgame_name> ::= "corporateCEOTM" | "baseTM" | ...
parseBoardGameName :: Parser String
parseBoardGameName = parseString "corporateCEOTM"
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

-- <boardgame> ::=  <boardgame_name> " " <price> "eur" " (contains: " <components> ")"
parseBoardGame :: Parser Product
parseBoardGame input = 
    and6 (\name price _ _ components _ -> BoardGame name price components) 
         parseBoardGameName 
         (parseChar ' ') 
         parsePrice 
         (parseString "eur") 
         (parseString " (contains: ") 
         parseComponents 
         (parseChar ')') 
         input


-- <product> ::= <boardgame> | <boardgame> "[includes: " <products> "]"
parseProduct :: Parser Product
parseProduct input = 
    parseBoardGame input `or2` 
    and4 (\boardGame _ products _ -> BoardGame' boardGame [] products) 
         parseBoardGame 
         (parseString " [includes: ") 
         parseProducts 
         (parseChar ']') 
         input


-- <component_name> ::= "tile" | "gameBoard" | ...
parseComponentName :: Parser String
parseComponentName = parseString "tile"
                `or2` parseString "gameBoard"
                `or2` parseString "playerBoard"
                `or2` parseString "card"
                `or2` parseString "marker"
                `or2` parseString "rules"    

{-
<tile> ::= <component_name>
<board> ::= <component_name>
<rules> ::= <component_name>
<card> ::= <component_name>
<marker> ::= <component_name>
<dice> ::= <component_name>
-}

parseComponent :: Parser Component
parseComponent [] = Left "Component is empty"
parseComponent input = parseComponentName input


-- <add_on_name> ::= "playerBoard" | "miniature" | ...
parseAddOnName :: Parser String
parseAddOnName [] = Left "Add on name is empty"
parseAddOnName = parseString "playerBoard"
             `or2` parseString "miniature"
             `or2` parseString "metalResource"
             `or2` parseString "cardSleeve"
             `or2` parseString "spaceInsert"

{-    
<space_insert> ::= <add_on_name> <price> "eur"
<miniature> ::= <add_on_name> <price> "eur"
<metal_resources> ::= <add_on_name> <price> "eur"
<card_sleeves> ::= <add_on_name> <price> "eur"
<player_board> ::= <add_on_name> <price> "eur"
-}
parseAddOn :: Parser AddOn      
parseAddOn [] = Left "Add on is empty"
parseAddOn input = 
    and3 (\name price _ -> AddOn name price) 
         parseAddOnName 
         parsePrice 
         (parseString "eur") 
         input




-- | An entity which represets user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.
-- The Query type representing user commands
data Query = RoundCommand Command
           | CheckShippingCommand Product
           | AddCommand Product Product
           | GiveDiscountCommand Product Float
           | BuyCommand Int Product
           | CompareCommand Product Product
  deriving Show

  
{-
-- | The instances are needed basically for tests
instance Eq Query where
  (==) _ _= False

instance Show Query where
  show _ = ""

-- | Parses user's input.
-- The function must have tests.
parseQuery :: String -> Either String Query
parseQuery _ = Left "Not implemented 2"

-- | An entity which represents your program's state.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data State

-- | Creates an initial program's state.
-- It is called once when the program starts.
emptyState :: State
emptyState = error "Not implemented 1"

-- | Updates a state according to a query.
-- This allows your program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition _ _ = Left "Not implemented 3"

-}