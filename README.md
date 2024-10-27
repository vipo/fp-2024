# Hotel Management System

## Overview

This is a functional programming project designed to mimic a hotel management system.

## Main BNF structure

```markdown

<add> ::= "ADD. " <hotelsID> 
<remove> ::= "REMOVE. " <hotelsID> 

<make_reservation> ::= "MAKE RESERVATION. " <guest> <hotel> <check_in> <check_out> <price>
<cancel_reservation> ::= "CANCEL RESERVATION. " <reservationID>
<add_additional_guest> ::= "ADD ADDITIONAL GUEST. " <guest> <reservationID>

<check_in> ::= "CHECK IN: " <date> " " <time> ". "
<check_out> ::= "CHECK OUT: " <date> " " <time> ". "
<guest> ::= "GUEST: " <name> " " <surname> ". "

<hotel> ::= "HOTEL: " <text> ". " |  <hotel> "CHAIN OF " <hotel> | <hotel> <floors>
<floors> ::= <floor> | <floor> <floors>
<floor> ::= "FLOOR: " <number> ". " <rooms>
<rooms> :: <room> | <room> <rooms>
<room> ::= "ROOM: " <number> ". " | <room> "ROOM SECTION " <room> | <room> <amenities> ". "

<price> ::= "PRICE: " <number> ". "
<hotelsID> ::= <number>
<reservationID> :: <number>

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

* `add` - adds a specific hotel entity. The entity can include a hotel, a specific room within a hotel, special properties.

    Example:
    ```
    ADD. HOTEL: Medis. CHAIN OF. HOTEL: Medis. FLOOR: 1. ROOM: 105. AMENITIES: Balcony, TV. 
    ```
* `remove` - removes a hotel entity from the available hotel list.

    Example:
    ```
    REMOVE. 1. 
    ```
* `make_reservation` - makes a reservation of a corresponding hotel room. The hotel room must exist in the available hotel list and it is called by inputting the same hotel. Future configurations could be made for the reservation to call on the ID. 
    
    Example:
    ```
    MAKE RESERVATION. GUEST: ELVINAS SVILPA. HOTEL: Medis. CHAIN OF. HOTEL: Medis. FLOOR: 1. ROOM: 105. AMENITIES: Balcony, TV. CHECK IN: 2024-09-16 20:20. CHECK OUT: 2024-09-20 12:00. PRICE: 510.

    ```
* `cancel_reservation` - cancels a reservation based on its reservation ID.

    Example:
    ```
    CANCEL RESERVATION. 1. 
    ```
* `add_additional_guest` - adds an additional guest to a room based on its reservation ID.

    Example:
    ```
    ADD ADDITIONAL GUEST. GUEST: Antrasis Elvinas. 1. 
    ```


### Details

Hotel managers also have the ability to specify check-in (check-out) dates, times and amenities, price.


