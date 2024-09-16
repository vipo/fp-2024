# Hotel Management System

## Overview

This is a functional programming project designed to mimic a hotel management system.

## Main BNF structure

```markdown

<add_hotel_room> ::= "ADD\n" <hotel> 
<remove_hotel_room> ::= "REMOVE\n" <hotel> 

<make_reservation> ::= "MAKE RESERVATION\n" <guest> <hotel> <check_in> <check_out> <price>
<cancel_reservation> ::= "CANCEL RESERVATION\n" <hotel>
<add_additional_guest> ::= "ADD ADDITIONAL GUEST\n" <guest> <hotel>

<check_in> ::= "CHECK IN: " <date> " " <time> "\n"
<check_out> ::= "CHECK OUT: " <date> " " <time> "\n"
<guest> ::= "GUEST: " <name> " " <surname> "\n"

<hotel> ::= "HOTEL: " <text> "\n" |  <hotel> "CHAIN OF " <hotel> | <hotel> <floor>
<floor> ::= "FLOOR: " <number> "\n" <room>
<room> ::= "ROOM: " <number> "\n" | <room> "ROOM SECTION " <room> | <room> <amenities> "\n"

<price> ::= "PRICE: " <number> "\n"

<amenities> ::= "AMENITIES: " <amenity> | <amenities> ", " <amenity>
<amenity> ::= "TV" | "WI-FI" | "MINI-BAR" | "BALCONY" | "AC"

<number> ::= [0-9]+
<text> ::= ( [a-z] | [A-Z])+
<digit> ::= [0-9]
<char> ::= ( [a-z] | [A-Z])


<name> ::= <text>
<surname> ::= <text>
<date> ::= <digit> <digit> <digit> <digit> "-" <digit> <digit> "-" <digit> <digit>
<time> ::= <digit> <digit> ":" <digit> <digit>
```

### Commands

* `add_hotel_room` - adds a room to a specific hotel

    Example:
    ```
    ADD
    HOTEL: Klevas
    CHAIN OF HOTEL: Medis
    FLOOR: 1
    ROOM: 105
    Amenities: BALCONY, TV
    ```
* `remove_hotel_room` - removes a room from a specific hotel

    Example:
    ```
    REMOVE
    HOTEL: Klevas
    CHAIN OF HOTEL: Medis
    FLOOR: 5
    ROOM: 515
    ROOM SECTION ROOM: 2
    AMENITIES: TV, WI-FI
    ```
* `make_reservation` - makes a reservation of a corresponding room. 
    
    Example:
    ```
    MAKE RESERVATION
    GUEST: ELVINAS SVILPA
    HOTEL: AZUOLAS
    FLOOR: 4
    ROOM: 414
    AMENITIES: MINI-BAR, BALCONY, AC
    CHECK IN: 2024-09-16 15:00
    CHECK OUT: 2024-09-20 12:00
    PRICE: 510

    ```
* `cancel_reservation` - cancels a reservation
* `add_additional_guest` - adds an additional guest to a room


### Details

Hotel managers also have the ability to specify check-in (check-out) dates, times and amenities, price.


