# Hotel Management System

## Overview

This is a functional programming project designed to mimic a hotel management system.

## Main BNF structure

```markdown
<command> ::=  <add_room> | <remove_room> | <make_reservation> | <cancel_reservation> | <add_additional_guest>

<add_room> ::= "ADD" <room>
<remove_room> ::= "REMOVE" <room>

<make_reservation> ::= "MAKE RESERVATION" <guest_name> <digit+> <check_in> <check_out>
<cancel_reservation> ::= "CANCEL RESERVATION" <digit+>
<add_additional_guest> ::= "ADD ADDITIONAL GUEST" <guest_name> <digit+>

<check_in> ::= "CHECK IN" <date> <time>
<check_out> ::= "CHECK OUT" <date> <time>
<guest_name> ::= "Guest:" <name> <surname>
<room> ::= "Room:" <digit+> | "Room:" <digit+> <amenities>
<amenities> ::= <amenity> | <amenity> "," <amenities>
<amenity> ::= "TV" | "Wi-Fi" | "Mini bar" | "Balcony" | "AC"

<digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
<char> ::= "a" | "b" | ... | "z" | "A" | "B" | ... | "Z"

<name> ::= <char+>
<surname> ::= <char+>
<date> ::= <digit> <digit> <digit> <digit> "-" <digit> <digit> "-" <digit> <digit>
<time> ::= <digit> <digit> ":" <digit> <digit>

```

### Commands

* `add_room` - adds a guest to the hotel
* `remove_room` - removes a room from the hotel
* `make_reservation` - makes a reservation of a corresponding room 
* `cancel_reservation` - cancels a reservation
* `add_additional_guest` - adds an additional guest to a room

### Details

Hotel managers also have the ability to specify check-in (check-out) dates,times and amenities.


