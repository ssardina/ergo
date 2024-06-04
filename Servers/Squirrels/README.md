# Squirrels

This is an adaptation/ripoff of the [Monty Karel](http://csis.pace.edu/~bergin/MontyKarel) robot world written by Joseph Bergin and colleagues in Python.

In our case, it is a world server written in Racket Scheme (not ERGO) that can be interacted with by up to four independent agents over a TCP connection. The interaction with ERGO is described in Chapter 7.

This directory contains the following:

- `figs/`: A subdirectory of bitmap images
- `sw-server.scm`: The complete implementation in Scheme
- `wall-follower.scm`: A sample agent in Scheme (not ERGO)

## SUMMARY

The programmed agents are imagined to be squirrels that need to move around on a two-dimensional grid and gather acorns.  Squirrels have both effectors (to do things in the world) and sensors (to gather information). Everything is known to the squirrels at the outset except for the locations of the acorns, wall obstacles, and other squirrels.  This can be thought of as a game where the goal is to be the first squirrel to gather 4 acorns.  (By analogy with the game of duplicate bridge, a duplicate version of the squirrel game would start different agents in the same initial world to see who best copes with the distribution of acorns and walls.)


## INSTALLING THE SQUIRREL WORLD

You need Racket Scheme <http://racket-lang.org/>.  I used version 5.2.  Other
versions may work too.  What follows are instructions for using the program
under Linux.  Using it with Windows or a Mac is similar.

Confirm that Racket is installed properly:

```shell
Linux%> racket -e '(+ 3 4)'
7
```

If this works, then try starting the Squirrel World (called "`sw-server.scm`") by typing this at the command-line:

```shell
Linux%> racket -fm sw-server.scm [<seed>]
```

This should start the simulation window.  You should see a default 30x50 grid, with squirrel icons located at each of the four corners and a smattering of acorns.  Type a "q" in this window to close the window and stop the program. (The seed number can be used to restart a game in the same configuration.)

Now you are ready to try some squirrel agents.  Start the server again and open another terminal window on the same machine. In this window, connect to the server using telnet at port 8123:

```shell
Linux%> telnet localhost 8123
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
Wally
```

If this works, type the command "forward" in the telnet session. It should respond with "`ok`".

```
forward
ok
```

Look at the server window. One of the squirrels should have moved 1 unit forward. Type a "q" in the server window to abort it and the telnet session.

Finally, you are ready to try a very simple automated agent provided called "`wall-follower`".  Restart the server, and in the other terminal window, start this squirrel agent as follows:

```shell
Linux%> racket -fm wall-follower.scm
Made network connection
Start wall following
```

In the server window, you should see one of the squirrels slowly inching itself forward until it reaches a wall.  At that point, it should run back home, and start inching towards a second wall to its right.  When it finds it, it should start running back and forth between the two walls.  This is all it does.  You can get the second, third, and fourth squirrel to do the same thing by opening more terminal windows and starting them like the first one. Eventually all squirrels will run out of energy, turn grey, and stop moving, but you can abort the session before then by typing "q" in the server window.

That's it. You're up and running.  Good luck!

## CREATING SQUIRREL AGENTS

Squirrel agents are simply programs that communicate with the server, `sw-server.scm`, over TCP on port 8123.  They can be written in any language that supports TCP communication, like Python, C, Java or Prolog.  Some of the details of this communication can be inferred from the `wall-follower.scm`
program, but here is a more complete story.

After a client connects on port 8123, the server sends the squirrel name to the client.  (It's one of Nutty, Skwirl, Edgy, or Wally, depending on whether it starts facing N, S, E or W.)  From then on, the client may send any number of actions each of which will cause the server to respond with a single reply,
after a delay corresponding to the duration of the action.  The actions and replies are presented below.  Sending an action is normally done by writing the action to the TCP port followed by an end-of-line and a command that flushes the output.  Receiving a reply is done by reading from the TCP port.

A client would normally wait for the response from the server before sending the next action, but it need not. However, the server will only read the next action sent by the client after it has sent the reply to the previous one. The server interacts with each client on a different thread, so not sending an
action causes no delay for the other clients.  If at any point the client closes the connection, it cannot be restarted.

The actions and their responses are as follows: (Note: a response of "`fail`" simply means that the action did not have its normal intended effect, although it continues to have the normal duration.)


| Action  | Response                                                                         |
| :------ | :------------------------------------------------------------------------------- |
| left    | ok                                                                               |
| right   | ok                                                                               |
| forward | ok or fail                                                                       |
| pick    | ok or fail                                                                       |
| drop    | ok or fail                                                                       |
| eat     | ok or fail                                                                       |
| build   | ok or fail                                                                       |
| feel    | an integer from 0 to 100                                                         |
| look    | wall or nothing                                                                  |
| smell   | a list (n m) where n and m are integers >= 0 n is for acorns, m is for squirrels |
| listen  | a list whose elements are lists (i j), where i and j are integers                |


In addition, a client can send "`quit`" and the server will terminate the connection.  Anything else, such as "`help`", will cause the server to respond with the list of actions above.

A client squirrel can decide what actions to send any way it wants.  All the information in the sw-server.scm program (for example, the size of the grid, the number of acorns, how far other squirrels can be heard etc) is fair game.

Note that each squirrel starts in a corner with a wall behind it and to its left. What the squirrel will not know at the start is the location of the acorns and the position and length of the wall segments, which are distributed uniformly at random.  In some cases, the distribution of acorns and walls is not "fair" including even boxing the squirrel in. It is up to the squirrel to
do the best it can even in such situations.


## RULES OF THE GAME

Squirrels and acorns live in a world divided into a large rectangular grid. Each acorn and squirrel is located at some point on the grid, and each point on the grid can contain any number of squirrels and acorns.  Between any two adjacent grid points there may be a wall that blocks passage between the two points.  A wall also surrounds the entire world.

Acorns are completely passive.  Squirrels on the other hand have a number of actions at their disposal which are either ordinary actions (that can change the world) or sensing actions (that provide information to the squirrel).

Each action has a fixed duration.  Squirrels have an energy level which is how many units of time they have left before they expire.  Squirrels start at the maximum level, which is 3000 units.  Energy level decreases as time advances, but can be increased by eating an acorn.

Squirrels can pick up acorns and carry up to 2 of them.  They can also drop an acorn they are holding.  Squirrels can also build additional wall segments.

**GOAL OF THE GAME:** The first squirrel to get 4 acorns wins.

A squirrel has access to the following ordinary primitive actions with the durations as indicated:

- `left 1`
  - Effect: Turn to the left 90 degrees
- `right 1`
  - Effect: Turn to the right 90 degrees
- `forward 1`
  - Effect: If there is wall in front, then the squirrel bumps into the wall, is stunned, and loses 750 units of time. (See "`look`" below.) Otherwise the squirrel moves ahead 1 unit.
- `pick 30`
  - Effect: If there is an acorn present and no other squirrel present and the squirrel is not already holding two acorns, then the squirrel picks up the acorn.  (See "`smell`" below.) Otherwise, no effect.
- `drop 30`
  - Effect: If the squirrel is holding an acorn then she drops it. Otherwise, no effect.
- `eat 60`
   - Effect: If the squirrel is holding an acorn then she disposes of it, getting an energy boost of 2700 time units. Otherwise, no effect.
- `build 50`
  - Effect: The squirrel creates a wall segment 1 unit long right in front of her. (Note: a squirrel can box herself in and starve!)

and the following sensing primitive actions:

- `feel 1`
  - Sensing result: the squirrel's energy on a scale from 0% to 100%.
- `look 10`
  - Sensing result: is there a wall directly in front?
- `smell 4`
  - Sensing result: are there acorns or other squirrels right here?
- `listen  40`
  - Sensing result: a list of the relative coordinates of other squirrels who are nearby but not hiding behind a wall.




