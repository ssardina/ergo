This area contains ERGO code for an agent running in a simulated Squirrel
World (SW) as described in Chapter 7.  The SW system is explained elsewhere.
(Look for the file "sw-server.scm".)  This directory has the ERGO portion:

   - README.txt              this file
   - sw-bridge.scm           A Scheme program for communicating with SW
   - simple-main.scm         A simple squirrel agent for SW
   - random-main.scm         A squirrel agent that does a randomized search
   - systematic-main.scm     A squirrel agent that does a systematic search
   - systematic-bat.scm         subprogram for systematic-main.scm
   - systematic-procs.scm       subprogram for systematic-main.scm

In general, an ERGO agent will produce endogenous actions that the squirrels
in the SW must perform.  The SW will signal the occurence of exogenous actions
that the ERGO program can then take note of.

------ Writing a squirrel agent in ERGO

The first step is to make sure the SW server is working properly.  The example
"wall-follower" program should do its thing.  See the documentation there.

An ERGO program loads the file "sw-bridge" to communicate with a running SW
server over TCP.  This file defines the two interface functions required for
online execution.  The basic action theory of the ERGO program can define
whatever endogenous actions it wants.  The "sw-bridge" will send the following
ones to the SW:

   feel look smell listen left right forward pick drop eat build quit

(The "sw-bridge" ignores any other actions.)  The first four of these actions
(feel, look, smell, listen) are sensing actions that "sw-bridge" treats as
requests for exogenous reports.  If one is used in an ERGO program, the basic
action theory should also define the corresponding exogenous action:

   feel   ->  (set-energy! x)
   look   ->  (set-view! x)
   smell  ->  (set-aroma! x)
   listen ->  (set-sound! x)

The argument to the exogenous action will be the sensing result returned by
the SW server.  In other words, an endogenous "look" action will be followed
by an exogenous "(set-view! wall)" or "(set-view! nothing)". (It is up to the
ERGO program to decide how fluents change as the result of these actions.)

The file "simple-main.scm" contains a simple ERGO program of this form.
Assuming an SW server is running, it should produce the following output:

   > racket -l ergo -f simple-main.scm -m
   Connecting to the Squirrel World
   Wally starts on a 30x50 grid
   Starting monitors for 1 exogenous and 1 endogenous interfaces.

At this point, no further output is produced, but the squirrel called Wally
(or one of the others) should start slowly moving until it sees a wall, and
then run back and forth.  To stop Wally, type a "q" in the simulation window.
(Note that this squirrel agent ignores acorns.)
