module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = 
    [
        "exercise", "trainer", "show"
        ,   "chest", "arms", "legs", "back"
        ,   "reps", "sets"
        ,   "available", "assigned", "schedule"
        ,   "today", "week", "month"
    ]
