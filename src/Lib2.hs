{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Lib2
  ( Query(..),
    State(..),
    Item(..),
    Category(..),
    Storage(..),
    ItemWithId(..),
    emptyState,
    stateTransition,
    parseItem,
    parseStorage,
    parseAddStorage,
    parseSellItem,
    parseQuery,
    parseRestock,
    parseRemove,
    parseView,
    parseAddInt,
    parseString,
  ) where

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Avoid lambda" #-}
import Data.Char (isDigit)

-- | Data types for Queries and State
data Query
  = AddStorage Storage
  | SellItem String Int
  | ShowInventory
  | RestockItems String Int
  | RemoveItem String
  deriving (Eq, Show)

-- | Categories of items in the store
data Category = Fruits | Vegetables | Grains | Dairy | Meats | Beverages | CleaningProducts | PaperGoods
  deriving (Eq, Show)

-- | Items in the grocery store
data Item
  = Food Category String Int
  | Beverage String Int
  | HouseholdSupplies Category String Int
  deriving (Eq, Show)

data Storage = Storage [Item]
  deriving (Eq, Show)

-- <GroceryStore> ::= [<Item>]
data ItemWithId = ItemWithId
  { itemId :: Int,
    item :: Item
  }
  deriving (Eq, Show)

-- | Program state representing the inventory
data State = State
  { inventory :: [ItemWithId],
    nextId :: Int
  }
  deriving (Eq, Show)

-- | Initial program state
emptyState :: State
emptyState = State {inventory = [], nextId = 1}

-- | Basic parsers
type Parser a = String -> Either String (a, String)

and2' :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
and2' f p1 p2 input =
  case p1 input of
    Right (v1, r1) ->
      case p2 r1 of
        Right (v2, r2) -> Right (f v1 v2, r2)
        Left e2 -> Left e2
    Left e1 -> Left e1

and3' :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
and3' f p1 p2 p3 input =
    case p1 input of
        Right (v1, rest1) ->
            case p2 rest1 of
                Right (v2, rest2) ->
                    case p3 rest2 of
                        Right (v3, rest3) -> Right (f v1 v2 v3, rest3)
                        Left err -> Left err
                Left err -> Left err
        Left err -> Left err

or2' :: Parser a -> Parser a -> Parser a
or2' p1 p2 input =
  case p1 input of
    Right (v1, r1) -> Right (v1, r1)
    Left _ -> p2 input

or3' :: Parser a -> Parser a -> Parser a -> Parser a
or3' p1 p2 p3 input =
  case p1 input of
    Right (v1, r1) -> Right (v1, r1)
    Left _ -> case p2 input of
      Right (v2, r2) -> Right (v2, r2)
      Left _ -> p3 input

or5' :: Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a
or5' p1 p2 p3 p4 p5 input =
  case p1 input of
    Right (v1, r1) -> Right (v1, r1)
    Left _ -> case p2 input of
      Right (v2, r2) -> Right (v2, r2)
      Left _ -> case p3 input of
        Right (v3, r3) -> Right (v3, r3)
        Left _ -> case p4 input of
          Right (v4, r4) -> Right (v4, r4)
          Left _ -> p5 input

skipSpaces :: String -> String
skipSpaces = dropWhile (== ' ')



parseLiteral :: String -> Parser String
parseLiteral [] input = Right ([], input)
parseLiteral (x:xs) input =
  let input' = skipSpaces input
   in if not (null input') && head input' == x
        then parseLiteral xs (tail input')
        else Left $ "Expected " ++ (x:xs)

-- <FoodItems> ::= <Fruits> | <Vegetables> | <Grains> | <Dairy> | <Meats>
parseFoodCategory :: Parser Category
parseFoodCategory input =
  case parseLiteral "Fruits" input of
    Right (_, rest) -> Right (Fruits, rest)
    Left _ -> case parseLiteral "Vegetables" input of
      Right (_, rest) -> Right (Vegetables, rest)
      Left _ -> case parseLiteral "Grains" input of
        Right (_, rest) -> Right (Grains, rest)
        Left _ -> case parseLiteral "Dairy" input of
          Right (_, rest) -> Right (Dairy, rest)
          Left _ -> case parseLiteral "Meats" input of
            Right (_, rest) -> Right (Meats, rest)
            Left _ -> Left "Failed to parse food category"

-- <HouseholdSupplies> ::= <CleaningProducts> | <PaperGoods>
parseHouseholdCategory :: Parser Category
parseHouseholdCategory input =
    case parseLiteral "CleaningProducts" input of
      Right(_, rest) -> Right (CleaningProducts, rest)
      Left _ -> case parseLiteral "PaperGoods" input of
        Right (_, rest) -> Right (PaperGoods, rest)
        Left _ -> Left "Failed to parse household supplies category"

-- <Fruits> ::= <Apples> | <Bananas> | <Oranges>
parseFruits :: Parser String
parseFruits input =
  let input' = skipSpaces input
      (name, rest) = span (/= ' ') input'
    in if name == "Apples" || name == "Bananas" || name == "Oranges"
       then Right (name, skipSpaces rest)
       else Left "No such fruit allowed"

-- <Vegetables> ::= <Carrots> | <Potatoes> | <Spinach>
parseVegetables :: Parser String
parseVegetables input =
  let input' = skipSpaces input
      (name, rest) = span (/= ' ') input'
    in if name == "Carrots" || name == "Potatoes" || name == "Spinach"
       then Right (name, skipSpaces rest)
       else Left "No such vegetable allowed"

-- <Grains> ::= <Rice> | <Bread> | <Pasta>
parseGrains :: Parser String
parseGrains input =
  let input' = skipSpaces input
      (name, rest) = span (/= ' ') input'
    in if name == "Rice" || name == "Bread" || name == "Pasta"
       then Right (name, skipSpaces rest)
       else Left "No such grain allowed"

-- <Dairy> ::= <Milk> | <Cheese> | <Yogurt>
parseDairy :: Parser String
parseDairy input =
  let input' = skipSpaces input
      (name, rest) = span (/= ' ') input'
    in if name == "Milk" || name == "Cheese" || name == "Yogurt"
       then Right (name, skipSpaces rest)
       else Left "No such dairy allowed"

-- <Meats> ::= <Chicken> | <Beef> | <Fish>
parseMeat :: Parser String
parseMeat input =
  let input' = skipSpaces input
      (name, rest) = span (/= ' ') input'
    in if name == "Chicken" || name == "Beef" || name == "Fish"
       then Right (name, skipSpaces rest)
       else Left "No such meat allowed"

-- <Beverages> ::= <Soda> | <Juice> | <Water>
parseSpecificBeverage :: Parser String
parseSpecificBeverage input =
  let input' = skipSpaces input
      (name, rest) = span (/= ' ') input'
    in if name == "Soda" || name == "Juice" || name == "Water"
       then Right (name, skipSpaces rest)
       else Left "No such fruit allowed"

-- <CleaningProducts> ::= <Detergent> | <Soap>
parseCleaningProducts :: Parser String
parseCleaningProducts input =
  let input' = skipSpaces input
      (name, rest) = span (/= ' ') input'
    in if name == "Detergent" || name == "Soap"
       then Right (name, skipSpaces rest)
       else Left "No such cleaning product allowed"

-- <PaperGoods> ::= <PaperTowels> | <ToiletPaper>
parsePaperGoods :: Parser String
parsePaperGoods input =
  let input' = skipSpaces input
      (name, rest) = span (/= ' ') input'
    in if name == "PaperTowels" || name == "ToiletPaper"
       then Right (name, skipSpaces rest)
       else Left "No such paper goods allowed"

parseBeverage :: Parser Item
parseBeverage input =
  and3' (\_ name number -> Beverage name number) (parseLiteral "Beverage") parseSpecificBeverage parseAddInt input

parseHousehold :: Parser Item
parseHousehold input =
  and3' (\cat name number -> HouseholdSupplies cat name number) parseHouseholdCategory (or2' parseCleaningProducts parsePaperGoods) parseAddInt input

-- <Item> ::= <FoodItems> | <Beverages> | <HouseholdSupplies>
parseItem :: Parser Item
parseItem =
  or3'
    (and3' (\cat name number -> Food cat name number) parseFoodCategory (or5' parseFruits parseVegetables parseGrains parseDairy parseMeat) parseAddInt) 
    parseBeverage
    parseHousehold

parseString :: Parser String
parseString input =
  let input' = skipSpaces input
   in if null input'
        then Right ("", "")
        else if head input' == '"'
             then parseQuotedString (tail input')
             else let (str, rest) = span (\c -> c /= ' ' && c /= ',' && c /= '(' && c /= ')') input'
                  in Right (str, rest)
  where
    parseQuotedString [] = Left "Unexpected end of input in quoted string"
    parseQuotedString ('"' : rest) = Right ("", rest)
    parseQuotedString (x : rest) =
      case parseQuotedString rest of
        Right (str, rest') -> Right (x : str, rest')
        Left err -> Left err

parseAddInt :: Parser Int
parseAddInt input =
    let (digits, rest) = span isDigit (skipSpaces input)
        baseValue = 0
    in if null digits
        then Right(baseValue, rest)
        else Right(read digits, rest)

parseStorage :: Parser Storage
parseStorage input = 
      case parseMultipleItems input of
        Right (items, rest') -> Right (Storage items, rest')
        Left _-> Left  "Likely case of mistyping"

-- <Storage> ::= <Item> | <Storage> <Item>
parseMultipleItems :: Parser [Item]
parseMultipleItems input =
  case parseItem input of
    Right (item, rest) ->
      case parseMultipleItems rest of
        Right (items, rest') -> Right (item : items, rest')
        Left _ -> Right ([item], rest)
    Left _ -> Left "Failed to parse items in storage"


parseAddStorage :: Parser Query
parseAddStorage = and2' (\_ storage -> AddStorage storage) (parseLiteral "add_storage") parseStorage

parseInt :: Parser Int
parseInt input =
  let (digits, rest) = span isDigit (skipSpaces input)
   in if null digits
        then Left "Expected an integer that is positive"
        else Right (read digits, rest)

parseView :: Parser Query
parseView input =
  case parseLiteral "show_store" (skipSpaces input) of
    Right (_, rest) -> Right (ShowInventory, rest)
    Left _ -> Left "Expected 'show_inventory'"

searchMatches :: ItemWithId -> String -> Bool
searchMatches (ItemWithId _ item) search = itemNameMatches item search

itemNameMatches :: Item -> String -> Bool
itemNameMatches (Food _ name _) search = name == search
itemNameMatches (Beverage name _) search = name == search
itemNameMatches (HouseholdSupplies _ name _) search = name == search

restockItem :: ItemWithId -> Int -> ItemWithId
restockItem (ItemWithId id (Food cat name qty)) quantity =
    ItemWithId id (Food cat name (qty + quantity))
restockItem (ItemWithId id (Beverage name qty)) quantity =
    ItemWithId id (Beverage name (qty + quantity))
restockItem (ItemWithId id (HouseholdSupplies cat name qty)) quantity =
    ItemWithId id (HouseholdSupplies cat name (qty + quantity))

parseRestockInventory :: [ItemWithId] -> String -> Int -> [ItemWithId]
parseRestockInventory [] _ _ = []
parseRestockInventory (x:xs) search number =
  if searchMatches x search
    then restockItem x number : parseRestockInventory xs search number
    else x : parseRestockInventory xs search number

parseSellInventory :: [ItemWithId] -> String -> Int -> Either String [ItemWithId]
parseSellInventory [] _ _ = Right []
parseSellInventory (x:xs) search number = 
  if searchMatches x search
    then case sellItem x number of
      Right updatedItem -> Right (updatedItem : xs)
      Left err -> Left err
    else case parseSellInventory xs search number of
      Left err -> Left err
      Right updatedRest -> Right (x : updatedRest)

sellItem :: ItemWithId -> Int -> Either String ItemWithId
sellItem (ItemWithId id (Food cat name qty)) quantityToSell =
    let newQty = qty - quantityToSell
    in if newQty >= 0 
        then Right (ItemWithId id (Food cat name newQty)) 
        else Left ("Insufficient stock for item: " ++ name)
sellItem (ItemWithId id (Beverage name qty)) quantityToSell =
    let newQty = qty - quantityToSell
    in if newQty >= 0 
        then Right (ItemWithId id (Beverage name newQty)) 
        else Left ("Insufficient stock for item: " ++ name)
sellItem (ItemWithId id (HouseholdSupplies cat name qty)) quantityToSell =
    let newQty = qty - quantityToSell
    in if newQty >= 0 
        then Right (ItemWithId id (HouseholdSupplies cat name newQty)) 
        else Left ("Insufficient stock for item: " ++ name)


parseRestock :: Parser Query
parseRestock input = 
  and3' (\_ name number -> RestockItems name number) (parseLiteral "restock") parseString parseInt input


parseSellItem :: Parser Query
parseSellItem input = 
  and3' (\_ name number -> SellItem name number) (parseLiteral "sell") parseString parseInt input

parseRemove :: Parser Query
parseRemove input =
  and2' (\_ itemName -> RemoveItem itemName) (parseLiteral "remove_item") parseString input
parseQuery :: String -> Either String Query
parseQuery input =
  case or5' parseView  parseAddStorage parseSellItem parseRestock parseRemove input of
    Right (query, _) -> Right query
    Left _ -> Left "Failed to parse: Unknown command given"

stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition st query = case query of
  AddStorage (Storage items) ->
    let
      itemsAreEquivalent :: Item -> Item -> Bool
      itemsAreEquivalent (Food cat1 name1 _) (Food cat2 name2 _) = cat1 == cat2 && name1 == name2
      itemsAreEquivalent (Beverage name1 _) (Beverage name2 _) = name1 == name2
      itemsAreEquivalent (HouseholdSupplies _ name1 _) (HouseholdSupplies _ name2 _) = name1 == name2
      itemsAreEquivalent _ _ = False

      itemExists :: Item -> [ItemWithId] -> Bool
      itemExists _ [] = False
      itemExists itemToCheck (ItemWithId _ invItem : rest) =
        itemsAreEquivalent itemToCheck invItem || itemExists itemToCheck rest

      filterNewItems :: [Item] -> [ItemWithId] -> [Item]
      filterNewItems [] _ = []
      filterNewItems (item:rest) inventory =
        if itemExists item inventory
          then filterNewItems rest inventory
          else item : filterNewItems rest inventory

      assignIds :: [Item] -> Int -> [ItemWithId]
      assignIds [] _ = []
      assignIds (item:rest) currentId =
        ItemWithId currentId item : assignIds rest (currentId + 1)

      uniqueItems = filterNewItems items (inventory st)
      newItems = assignIds uniqueItems (nextId st)      
      newInventory = inventory st ++ newItems
      newState = st { inventory = newInventory, nextId = nextId st + length uniqueItems }
    in
      if null uniqueItems
      then Right (Just "No new items to add. All items already in inventory.", st)
      else Right (Just ("Added to inventory: " ++ show uniqueItems), newState)

  SellItem item' qty' -> 
    case parseSellInventory (inventory st) item' qty' of
      Right updatedInventory -> 
        let newState = st { inventory = updatedInventory }
        in Right (Just ("Sold item: " ++ item' ++ " with quantity " ++ show qty'), newState)
      Left err -> Right (Just err, st)

  RestockItems item' number' ->
    let updatedInventory = parseRestockInventory (inventory st) item' number'
        newState = st { inventory = updatedInventory }
    in if updatedInventory == inventory st
       then Right (Just ("Item not found: " ++ item'), st)
       else Right (Just ("Restocked item: " ++ item' ++ " with quantity " ++ show number'), newState)

  RemoveItem itemName  ->
    let
      newInventory = filter (\(ItemWithId _ item) -> not (itemNameMatches item itemName)) (inventory st)
    in
      if length newInventory == length (inventory st)
      then Right (Just ("Item not found: " ++ itemName), st)
      else
        let newState = st { inventory = newInventory }
        in Right (Just ("Removed item with name: " ++ itemName), newState)
  ShowInventory ->
    let inventoryStr = showInventory (inventory st)
    in Right (Just ("Inventory list:\n" ++ inventoryStr ++ "\n"), st)

showInventory :: [ItemWithId] -> String
showInventory [] = "No more items in inventory."
showInventory (x:xs) = "Item ID: " ++ show (itemId x) ++ ", Item: " ++ show (item x) ++ "\n" ++ showInventory xs
