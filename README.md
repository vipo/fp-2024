# fp-2024

# Board game shop

<round_command> rounds up the number to two decimal spaces.

<check_shipping_command> checks if the product price is over 70. If yes, no shipping cost will be applied.

<add_command> add two product prices

<discount_command> gives a discount to a product

<buy_command> calculates the final price of the purchase

<compare_command> compares two product prices

## Recursion

### Implementing real life example:

  * Terraforming Mars Kickstarter edition: Corporate CEO
     1. Terraforming Mars
        1. Rules
        2. Game board
        3. 5 Player boards
        4. 17 Corporation Cards
        5. 208 Corporation Cards
        6. 8 refrence Cards
        7. 200 player Markers
        8. 200 Resource Markers
        9. 3 Game board Markers
        10. 9 Ocean tiles
        11. 60 Greenery/city tiles
        12. 11 Special tiles
        13. First player Marker
     2. Terraforminh Mars Big Box
        1. 3D printed tiles
          1. 24 City Tiles
          2. 40 Greenery Tiles
          3. 9 Ocean Tiles
          4. 14 Special Tiles
        2. Big Box promo cards
     3. Venus Next expansion
        1. 49 project cards
        2. 5 corporation cards
        3. Milestone tile
        4. Award tile
        5. Venus Board
        6. Venus scale marker
        7. Rules
     4. Turmoil expansion
        * ...
     6. Colonies expansion
        * ...
     5. Prelude expanion
        1. Prelude 1 expanion
          * ...
        2. Prelude 2 expanion
          * ...
     7. Ellas & Hellium map expansion
        * ...


### To transfer this to my repl, the commad would look like this:

TMCorporateCEO 224 eur [includes: baseTM 37 eur (contains: 1 rules, 1 gameBoard, 5 playerBoard, 233 cards, 401 marker, 80 tiles), bigBoxTM 114 eur (contains: 167 tile, 20 card), venusTMexp 26 eur (contains: 1 rules, 1 gameBoard, 2 tile, 54 card), turmoilTMexp 26 eur (contains: 1 rules, 1 gameBoard, 3 tile, 60 card), coloniesTMexp 26 eur (contains: 1 rules, 1 gameBoard, 5 tile, 40 card), preludeTMexp 38 eur (contains: 1 rules)[includes: prelude1TMexp (contains: 1 rules, 30 card), prelude2TMexp (contains: 1 rules, 32 card)], hellasTMexp 19 eur (contains: 1 rules, 2 gameBoard)]


### Upper command formated:

TMCorporateCEO 224 eur \
   [includes: \
      baseTM 37 eur \
         (contains: 1 rules, 1 gameBoard, 5 playerBoard, 233 cards, 401 marker, 80 tiles), \
      bigBoxTM 114 eur \
         (contains: 167 tile, 20 card), \
      venusTMexp 26 eur \
         (contains: 1 rules, 1 gameBoard, 2 tile, 54 card), \
      turmoilTMexp 26 eur \
         (contains: 1 rules, 1 gameBoard, 3 tile, 60 card), \
      coloniesTMexp 26 eur \
         (contains: 1 rules, 1 gameBoard, 5 tile, 40 card), \
      preludeTMexp 38 eur \
         (contains: 1 rules) \
            [includes: \
               prelude1TMexp \
                  (contains: 1 rules, 30 card), \
               prelude2TMexp \
                  (contains: 1 rules, 32 card) \
            ], \
      hellasTMexp 19 eur \
         (contains: 1 rules, 2 gameBoard)  \
   ] \



### Random example provided by "https://bnfplayground.pauliankline.com/":

ellas&hellasTMexp 8.1eur (contains: 0 rules)[includes: venusTMexp 31eur (contains: 32 card)[includes: crisisTMAEexp 7.1eur (contains: 79 rules), baseTMAE 4.3eur (contains: 7 card, 8 marker, 2 rules)[includes: venusTMexp 93.68eur (contains: 5 tile)]], foundationsTMAEexp 67.9eur (contains: 64384 tile)]


### Upper command formated:
<pre>
ellas&hellasTMexp 8.1eur (contains: 0 rules) 
   [includes: 
   venusTMexp 31eur (contains: 32 card) 
      [includes: 
         crisisTMAEexp 7.1eur (contains: 79 rules), 
         baseTMAE 4.3eur (contains: 7 card, 8 marker, 2 rules) 
            [includes: 
               venusTMexp 93.68eur (contains: 5 tile) 
            ] 
      ], 
      foundationsTMAEexp 67.9eur (contains: 64384 tile) 
   ] 

</pre>

