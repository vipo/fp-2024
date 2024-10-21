module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = [
    "hotel",
    "floor",
    "room",
    "guest",
    -- commands
    "add",
    "remove",
    "make_reservation",
    "cancel_reservation",
    "add_additional_guest",
    "check_in",
    "check_out",

    -- details
    "amenities",
    "amenity",
    "name",
    "surname",
    "time",
    "price",

    "digit",
    "char",
    "text",
    "number"
    ]
