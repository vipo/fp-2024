module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = [
    --command
    "borrow",
    "return",
    "add-reader",
    "remove-reader",
    "add-book",
    "remove-book",
    "merge",
    
    --genre
    "fantasy",
    "detective",
    "scientific",
    "dictionary",

    --audience
    "children",
    "teenager",
    "adult"
    ]
