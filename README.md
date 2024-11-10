# FP-2024 | Vehicle Garage

### To get started, you first need to open the project using Visual Studio Code and having Docker Desktop
1. `Ctrl + Shift + P`
2. `Dev Containers: Open Folder in Container`

### To Build & Test the Project, run the following commands
1. `stack build`
2. `stack test`

### To Execute a Lecture Example
1. `stack ghci`
2. `:l src/Lessons/Lesson01.hs`

### Lib2.hs BNF Changes
Added View command to BNF for a more extensive display of inventory, clearly defined what is a digit and what is a number.

### Batch Queries
From Lib3 it is possible to provide queries as a batch.
After loading the program, you can run it using `stack run fp2024-three`, then write the following commands:
```
>>> :paste
-- Entering multi-line mode. Press <Ctrl-D> to finish.
| BEGIN
| add_vehicle(Car, "Mini", 2009, 240000 km);
| perform_maintenance(Car, OilChange, 2 days);
| inventory(Car);
| sell_vehicle(Car, "Mini", 2009, 5555.55);
| END
| 
Added Car Mini (2009)
Performed OilChange on Car for Days 2
Inventory for Car:
Mini (2009)

Sold Car Mini (2009) for $5555.55
```