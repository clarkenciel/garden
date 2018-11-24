# garden-hs

`garden-hs` is a haskell implementation (in various stages of completion) of a fun idea i had once for "word
gardens": small digital grids into which you could "plant" text that would grow outward using markov processes.
Your "plants" would merge as they overlapped and slowly die off.

So far this is just a little command line "proof of concept" with the growth and markov logic working.
Next, I'd like to get the life-cycle stuff implemented, and then work on things like persistence and eventually
a web server implementation.


**NB** This program uses [stack](https://github.com/commercialhaskell/stack) as a build tool and you'll need to have
that tool installed to build and run the program.

## Build

```bash
$ stack build
```

## Run

```bash
$ stack run
```
