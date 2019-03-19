This directory contains the files to run a real-time CarChase Game under the
Unity 3d system (https://unity3d.com/).  The game involves two cars, A patrol
car under user control and a joyride car under ERGO control, as described more
fully in Chapter 9.

The directory contains the following:

    - README.txt                    This file
    - CarChase/                     The Unity 3d files.

To run the game server, first copy the folder CarChase to somewhere where
files and folders can be freely written.  Then start Unity 3d, pointing it to
this copy of CarChase.  Unity 3d will then create a number of other files and
folders (with names like Library, ProjectSettings, UnityProjectManager) in the
CarChase folder.  When it is done, ask Unity 3d to build an application.

When this game server application is run, there will be two cars in a scene, a
patrol car and a joyride car.  The patrol car can be driven using the arrow
keys on the keyboard.  The joyride car is waiting for a client connection over
TCP (port 8123) and will be driven by commands over the TCP link.

To test this, connect to the game server using telnet, and give the command
"go!".  This will cause the joyride car to go forward until it crashes.

The joyride car accepts the following commands:

   right-turn!          start turning the car to the right
   left-turn!           start turning the car to the left
   straight!            stop turning the car
   go!                  apply force forward
   reverse!             apply force backward
   stop!                stop applying force

In addition, it will signal over TCP if there is an obstacle ahead with

   (object-detect! d)

where d is the observed distance. (d=0 means the object is no longer ahead).

The game terminates when some object (the patrol car) taps the rear bumper of
the joyride car.

