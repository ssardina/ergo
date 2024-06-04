# AN ELEVATOR SIMULATION IN RACKET

This is a very simple elevator simulation written in Racket Scheme (independent of ERGO).  There is an elevator located on a certain floor with a number of call buttons illuminated.  On input from the user, the elevator moves up and down and can extinguish call button lights.  That's about it.

See Chapter 6 on how to control this simulation from ERGO.

This directory contains the following:

- `elevator-server.scm`: The entire Scheme program
- `ele.png`: A graphics file used by the simulation
- `red.jpeg`: A graphics file used by the simulation

To run the simulation, you need Racket Scheme <http://racket-lang.org/>.  I used version 5.2.  Other versions may work too.  What follows are instructions for using the program under Linux.  Using it with Windows or a Mac is similar.

Confirm that Racket is installed properly:

```shell
Linux%> racket -e '(+ 3 4)'
7
```

If this works, then start the elevator (called "`elevator-server.scm`") by typing something like this at the command-line:

```shell
Linux%> echo "((up 10) (down 6))" | racket -fm elevator-server.scm
```

This should start the simulation window.  You should see the elevator inching its way up to floor 10, down to floor 6, and then quitting.  You can also abort the session before the end by typing "q" in the server window.

The elevator simulation expects to read a single list of actions to perform on standard input.  The actions are

- `(up n)`: go up to floor `n`
- `(down n)`: go down to floor `n`
- `(turnoff n)`: turnoff call button `n`

No error checking is done.



