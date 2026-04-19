# 🚚 VENDOR EXERCISE --- COBOL 

## Overview
(I followed a step by step exercise for the base flow, but I implemented parts independently and adapted the logic.)
VNDINDX02 is a COBOL program that manages a file using indexed file operations.
It allows the user to add, modify, view, and delete records containing: 
    - VENDOR NUMBER --> Primary Key
    - Vendor Name, Addrees 1, Address 2 (optional), city, state, zip code, contanct name and phone number.

## Features
- Add new records (with duplicate check)
- Change existing state records
- Look up and display records
- Delete records with confirmation
- Input validation for menu choices and fields
  
## How It Works
The program opens an indexed file in I-O mode.
A menu is displayed to the user with options:
  1 → Add records
  2 → Change a record
  3 → Delete records
  4 → Inquiry a record
  5 → Create a report file
  6 → Display all records
  0 → Exit program
  
The user interacts via terminal input. 
Based on the ooption of the user i used the CALL statement to call different SUBPROGRAM. 
  Example. If the user input is 1 -> the SUBPROGRAM ADDREC02 is called
  
Each operation loops until the user enters ZERO as VENDOR NUMBER to stop.

## Data Handling
VNDINDX02 is the MAIN program

## File operations use:
  - READ (with key validation)
  - WRITE (prevents duplicates)
  - REWRITE (for updates)
  - DELETE (for removals)
    
## Notes
Error handling is basic (displays messages on invalid operations).
