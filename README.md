# fireflower

## Overview
Fireflower is a bot-in-progress that plays (some variations of) the game Hanabi. Right now, in early stages and focusing mainly on two-player.
Currently:
* Wins 2-player around 55% of the time, with an average score of around 21.
* Wins 3-player around 38% of the time, with an average score of around 21.
* Wins 4-player around 15% of the time, with an average score of around 20.

(Regarding the average score - note that by default with the current rules implementation the game ends immediately if all copies of a card are discarded or if the game becomes unwinnable due to too many total discards, so the average scores here are lower than if this wasn't the case.)

## Getting started

### Setup
1. Install [sbt](http://www.scala-sbt.org/download.html)
2. Clone this github repo to any desired directory and navigate to that directory.
3. Run `sbt`.
4. Within sbt, run `compile` to build the Scala code
5. Within sbt, run `run` and select one of the options to run the Scala code.

### SBT memory issues

Note that to avoid memory leaks via Java's permanent generation in a long-running sbt process,
you may need to edit your sbt configuration (i.e. the sbt script installed at ~/bin/sbt) if
you have Java 1.7 or earlier. If you do encounter out-of-memory issues in sbt, try editing the script
to call Java with the following flags:

    -XX:+CMSClassUnloadingEnabled
    -XX:+UseConcMarkSweepGC
    -XX:MaxPermSize=1G

### Scala SBT file name length errors

If during a compile in SBT you encounter the error `filename too long` or similar, it may be due to sbt trying to generate a file whose name exceeds the max allowed filename length on your system. See if you can specify an override for your sbt install to cap the filename length it uses:

http://stackoverflow.com/questions/28565837/filename-too-long-sbt

## Code Overview

A quick overview of what files in src/main/scala/ contain what, in approximately dependency order:
* Infrastructure
  * RichTypes.scala - convenience functions
  * Rand.scala - basic system-independent random number generator
* Game Implementation
  * CommonTypes.scala - some simple types and type aliases
  * Color.scala - defines Color type for colors of Hanabi cards
  * Card.scala - defines Card type for Hanabi cards
  * Actions.scala - defines types for hints and actions
  * Rules.scala - defines the possible Hanabi rule sets and game parameters
  * Hand.scala - a simple mutable container for storing a player's hand.
  * SeenMap.scala - a mapping that stores the mapping of cardIds to cards based on what cards have been seen or not
  * Game.scala - the main game implementation and rules
  * Player.scala - interface that players of the game need to satisfy
  * Sim.scala - functions to actually run the game with a group of players
* Player Implementation
  * CardPropertyMap.scala - data structure to remember properties of cards, used in implementation of AI players
  * RandomPlayer.scala - a player that plays randomly
  * HeuristicPlayer.scala - a player that plays via hardcoded conventions and heuristics, with a bit of search
* Top-level
  * Sandbox.scala - driver for random experimentation and debugging
  * Test.scala - a standard runnable set of tests to evaluate player quality

## Contributors

* David Wu
