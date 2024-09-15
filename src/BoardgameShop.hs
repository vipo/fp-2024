module BoardgameShop (catanBase) where

-- Information
bundleDiscount :: Double
bundleDiscount = 15

shippingCost :: Double
shippingCost = 3.99
--

-- Primary functions
add :: Double -> Double -> Double
add a b = a + b    

checkShipping :: Double -> Double 
checkShipping a =
    if a >= 70 
        then a
    else add a shippingCost

buy :: Double -> Double
buy a = roundTo 2 (checkShipping a )

giveDiscountForBundle :: Double -> Double
giveDiscountForBundle a = a * ((100 - bundleDiscount) / 100)

roundTo :: Int -> Double -> Double
roundTo n x = (fromInteger $ round $ x * (10^n)) / (10.0^^n)
-- from https://www.tutorialspoint.com/haskell-program-to-round-a-number-to-n-decimal-places
--



-- Base game prices
catanBase :: Double
catanBase = 33.59

catanBase_GameOfThrones :: Double
catanBase_GameOfThrones = 79.00
--

-- Expansions for Catan prices
catanExp_5to6players :: Double
catanExp_5to6players = 25.99

catanExp_seafarers :: Double
catanExp_seafarers = 35.99

catanExp_citiesandknights :: Double
catanExp_citiesandknights = 32.50
--

-- Special bundle
catanWithAllExpBundle :: Double
catanWithAllExpBundle = giveDiscountForBundle (add catanBase (add catanExp_5to6players (add catanExp_seafarers catanExp_citiesandknights)))






