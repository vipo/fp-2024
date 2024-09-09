module Lib1
  ( completions,
  )
where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions =
  [ -- Actions
    "add_vehicle",
    "perform_maintenance",
    "sell_vehicle",
    "inventory",
    -- Vehicle Types
    "Car",
    "Truck",
    "Motorcycle",
    "SUV",
    -- Maintenance Types
    "OilChange",
    "TireRotation",
    "BrakeInspection",
    "EngineTuneUp",
    -- Mileage
    "km",
    -- Duration
    "hours",
    "days"
  ]