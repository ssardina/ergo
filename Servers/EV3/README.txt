This directory contains Python code that can used to create a server that will
manage a LEGO robot on instructions from an ERGO program.  It assumes that the
EV3dev operating system has already been installed on a micro-SD card for the
EV3 Brick of the LEGO robot.  See http://ewww.ev3dev.org/ for instructions.

The use of this code with ERGO is described in Chapter 8 of the book. 
This directory contains the following:

    - README.txt                    This file
    - misc/                         Code to test motors/sensors (no TCP)
    - ergo_communications.py        Code to read/write over TCP
    - test_server.py                Code to confirm working TCP
    - manager1.py                   Code to confirm motor/sensor TCP control
    - delivery_manager0.py          Code for delivery robot of Chapter 8

All these files should be copied to the EV3 Brick for execution there.
