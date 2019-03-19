This area contains the ERGO code for a robot that runs under the Unity 3d game
environment as described in Chapter 9.  The Unity 3d part is explained
elsewhere. (Look for "CarChase".)  This directory has the ERGO portion:

    - README.txt              This file
    - u3d-bridge.scm          A Scheme program for communicating with Unity 3d
    - u3d-car.scm             An ERGO program for controlling a CarChase car

In general, the ERGO program will produce endogenous actions that the LEGO
robot must perform.  The LEGO robot will signal the occurence of exogenous
actions that the ERGO program must take note of.
