# Hotel Management System

## Overview

This is a functional programming project designed to mimic a hotel management system.

## Main BNF structure

```markdown

<add_room> ::= "ADD " <room>
<remove_room> ::= "REMOVE" <room>

<make_reservation> ::= "MAKE RESERVATION " <guest> " " <number> " " <check_in> " " <check_out>
<cancel_reservation> ::= "CANCEL RESERVATION " <number>
<add_additional_guest> ::= "ADD ADDITIONAL GUEST " <guest> " " <number>

<check_in> ::= "CHECK IN " <date> " " <time>
<check_out> ::= "CHECK OUT " <date> " " <time>
<guest> ::= "Guest: " <name> " " <surname>
<room> ::= "Room: " <number> | "Room: " <number> " " <amenities>
<amenities> ::= <amenity> | <amenity> ", " <amenities>
<amenity> ::= "TV" | "Wi-Fi" | "Mini bar" | "Balcony" | "AC"

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

* `add_room` - adds a room to the hotel
* `remove_room` - removes a room from the hotel
* `make_reservation` - makes a reservation of a corresponding room 
* `cancel_reservation` - cancels a reservation
* `add_additional_guest` - adds an additional guest to a room

### Details

Hotel managers also have the ability to specify check-in (check-out) dates, times and amenities.


