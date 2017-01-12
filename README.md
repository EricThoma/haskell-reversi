haskell-reversi
===============

A Reversi (technically Othello) engine written in Haskell during winter break 2014.

You play as white against the computer (black). Enter the square you want to move to in chess notation, e.g. d6.

The move generator is based off of my chess magic move generator. This engine serves as a proof of concept of that generator, while being much quicker to write than an actual chess engine. I hardcoded the magic numbers into the engine, for simplicity and so they don't have to be regenerated each run.

Dependencies: game-tree and timeit -- available from Hackage. Simple build: type "ghc *.hs -O2" while in haskell-reversi folder.

Use/change this as you like. Nothing guaranteed.
