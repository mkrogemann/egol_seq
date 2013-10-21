egol
====

[![Build Status](https://travis-ci.org/mkrogemann/egol.png?branch=master)](https://travis-ci.org/mkrogemann/egol)

Conway's Game of Life in Erlang - just to learn a bit about Erlang/OTP, functional problem solving and tools like rebar and QuickCheck.


What's next?
============

* Convert into an OTP application
* Print game state to console
* Alternative data structures (gb_tree, bit vector, ...)
* Benchmarking
* More tools (dyalizer, ...)
* Concurrency (cell == process?) -> each cell would need to figure out how to ask its neighbors for their states. Also must not switch to new value immediately but only after all cells have computed new state, then all switch at once.

