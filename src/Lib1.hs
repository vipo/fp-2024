module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = [
    -- Entities
    "book",
    "title",
    "author",
    "genre",
    "isbn",
    "publisher",
    "publication-date",
    "borrower",
    "due-date",
    "library",
    "shelf",
    "category",

    -- Genres
    "Fiction",
    "Non-Fiction",
    "Mystery",
    "Fantasy",
    "Science Fiction",
    "Biography",
    "Historical",
    "Thriller",

    -- Commands
    "addBook",
    "removeBook",
    "borrowBook",
    "returnBook",
    "searchBook",
    "listAvailableBooks",
    "listBorrowedBooks",
    "checkDueDate"
    ]

