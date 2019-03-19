This area contains the ERGO code for a LEGO robot that is managed by an EV3
Brick as described in Chapter 8.  The EV3 brick is explained elsewhere.  (Look
for the file "manager1.py".)  This directory has the ERGO portion:

    - README.txt              this file
    - tag-bridge.scm          A Scheme program for communicating with an EV3
    - test-manager1.scm       An ERGO program for testing the EV3 over TCP
    - delivery-main.scm       An ERGO program for delivery with the LEGO robot
    - delivery-map.scm          a subprogram for delivery-main
    - delivery-bat.scm          a subprogram for delivery-main

In general, the ERGO program will produce endogenous actions that the LEGO
robot must perform.  The LEGO robot will signal the occurence of exogenous
actions that the ERGO program must take note of.
