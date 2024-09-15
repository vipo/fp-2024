# fp-2024

This domain is trying to simulate a boardgames shop. 
For now, only the Catan base game is sold with expansions. 

If customer wants to buy a boardgame or it's expansios. He writes:
    > buy "product name"
     This function automaticlly check if free shipping will be applied by running:
        > checkShipping "price" 
     In addtion, rounds up to n decimal spaces by runnig:
        > roundTo n "number"

The shop also offers special bundles, for instance, if customer wants to buy base Catan with all expansions. He writes:
    > buy catanWithAllExpBundle
     In addition to all the functions above, this function sums up all the prices of the wanted products by running (Here, recurssion is used): 
        > add a b
    Also, a discount is given by function:
        > giveDiscountForBundle







