This is an adaptation/ripoff of the Monty Karel robot world written by Joseph
Bergin and colleagues in Python (see http://csis.pace.edu/~bergin/MontyKarel).
In our case, it is a world server written in Racket Scheme (not ERGO) that can
be interacted with by up to four independent agents over a TCP connection.
The interaction with ERGO is described in Chapter 7.

This directory contains the following:

    - README.txt                    This file
    - rules.txt                     How the world works
    - figs/                         A subdirectory of bitmap images
    - install.txt                   How to get the world running
    - sw-server.scm                 The complete implementation in Scheme
    - protocol.txt                  Instructions for creating agents
    - wall-follower.scm             A sample agent in Scheme (not ERGO)

SUMMARY: the programmed agents are imagined to be squirrels that need to move
around on a two-dimensional grid and gather acorns.  Squirrels have both
effectors (to do things in the world) and sensors (to gather information).
Everything is known to the squirrels at the outset except for the locations of
the acorns, wall obstacles, and other squirrels.  This can be thought of as a
game where the goal is to be the first squirrel to gather 4 acorns.  (By
analogy with the game of duplicate bridge, a duplicate version of the squirrel
game would start different agents in the same initial world to see who best
copes with the distribution of acorns and walls.)
