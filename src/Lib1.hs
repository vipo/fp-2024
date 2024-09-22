module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions =
  [ -- Actions
    "add_item",
    "delete_item",
    "restock_item",
    "sell_item",
    "inventory",
    -- Food Items
    "Fruits",
    "Vegetables",
    "Grains",
    "Dairy",
    "Meats",
    -- Fruit Types
    "Apples",
    "Bananas",
    "Oranges",
    -- Vegetable Types
    "Carrots",
    "Potatoes",
    "Spinach",
    -- Grain Types
    "Rice",
    "Bread",
    "Pasta",
    -- Dairy Types
    "Milk",
    "Cheese",
    "Yogurt",
    -- Meat Types
    "Chicken",
    "Beef",
    "Fish",
    -- Beverages
    "Soda",
    "Juice",
    "Water",
    -- Household Supplies
    "CleaningProducts",
    "PaperGoods",
    -- Cleaning Products Types
    "Detergent",
    "Soap",
    -- Paper Goods Types
    "PaperTowels",
    "ToiletPaper"
  ]
