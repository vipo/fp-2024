module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = [
    --reader
    "borrow",
    "return",
    "add-reader",
    "remove-reader",
    "reader info",
    "reader-id",
    "name",

    --book
    "add-book",
    "remove-book",
    "title",
    "book-info",
    "author",
    "isbn",

    --general
    "list-readers",
    "list-books"
    ]
