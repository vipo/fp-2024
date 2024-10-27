module Lib1
    ( completions
    ) where


-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
--pakeist
completions =
  [
	-- Commands
	"print_info",
	"sell_artwork",
	"add_artwork",
	-- Art piece information
	"type",
	"title",
	"artist",
	"date",
	"price", 
	"description",
	-- Art piece types
	"Painting",
	"Photograph", 
	"Sculpture",
	"Digital",
	"Drawing", 
	"Sketch"
  ]