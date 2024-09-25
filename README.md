# fp-2024

## Setup

### To get started, you first need to open the project using Visual Studio Code and having Docker Desktop
1. `Ctrl + Shift + P`
2. `Dev Containers: Open Folder in Container`

### To Build & Test the Project, run the following commands
1. `stack build`
2. `stack test`


<GroceryStore> ::= <Storage> <FoodItems> <Beverages> <HouseholdSupplies>

<Item> ::= <FoodItems> | <Beverages> | <HouseholdSupplies>

<Programme> ::= "Add" <Storage> <Item> | "Delete" <Storage> <Item> | "Restock" <Storage> <Item> | "Sell" <Item> | "Exit"

<Storage> ::= <FoodItems> | <Storage> <Item>

<FoodItems> ::= <Fruits> | <Vegetables> | <Grains> | <Dairy> | <Meats>

<Fruits> ::= <Apples> | <Bananas> | <Oranges>

<Vegetables> ::= <Carrots> | <Potatoes> | <Spinach>

<Grains> ::= <Rice> | <Bread> | <Pasta>

<Dairy> ::= <Milk> | <Cheese> | <Yogurt>

<Meats> ::= <Chicken> | <Beef> | <Fish>

<Beverages> ::= <Soda> | <Juice> | <Water>

<HouseholdSupplies> ::= <CleaningProducts> | <PaperGoods>

<CleaningProducts> ::= <Detergent> | <Soap>

<PaperGoods> ::= <PaperTowels> | <ToiletPaper>
