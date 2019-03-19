Servers are programs that an ERGO program can interact with over TCP when it
is run online.  The idea is that ERGO produces endogenous actions as it
running which a server will consume and do something reasonable with.  Also,
the server may produce its own exogenous actions which ERGO will consume and
do something reasonable with.

This directory contains the following:

    - README.txt                    This file
    - generic-action-server.scm     Code for building Scheme-based servers
    - sonar-robot-server.scm        A simple robot server for Chapter 11
    - BasicElevator/                An online elevator server for Chapter 6 
    - Squirrels/                    A simulated world for Chapter 7
    - EV3/                          A LEGO robot server for Chapter 8
    - Unity3D/                      A Unity robot server for Chapter 9

Each subdirectory will contain its own explanatory README file.

