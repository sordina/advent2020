# Advent of Code 2020 Solutions

Various solutions to the 2020 problems. Golf style!

## Background

Original solutions were written as "oneliners" with a custom build of the
`hoe` tool, as well as several solutions in `perl` and `j`. You can find
these solutions in the ./oneliners/ directory of this project. Since this
makes it difficult to run them yourself, I've created a cabal project to
explicitly list all the dependencies, etc.

## Source

Source for solutions is found in `src/`. With part one and two located in the same AdventDAY.hs file.

## Running

All solutions are run by calling `cabal run advent2020 DAY[b]`.

Input is provided on STDIN except in a few cases indicated by the lack of `interact` in `Main.hs`.

Example: `cabal run advent2020 22b < advent22.input`.

## Favourite Solutions

* Day 6: Plane Seating - Binary Interpretation
* Day 17: Conway Cubes - List Comprenensions to the Rescue
* Day 19b: Parser Builder with Non-Determinism
* Day 24: Hexagon Grid - Coordinate transformation problem solved with Ren

## Solutions of Interest - Line Notes

* Day 1: Simple use of list-comprehensions.
* Day 2: Use of lens regex library.
* Day 3: Tabulated functions.
* Day 6: Direct binary interpretation.
* Day 7: Regexes with groups.
* Day 7: Use of Zoom from Lens.
* Day 10: Dynamic programming solution using Arrays for subsolutions.
* Day 12: Heavy use of stateful lenses. Tabulated functions.
* Day 13: Chinese remainder theoreb using `arithmoi` library. Needed a hint as I could only figure out that I needed the extended Euclid algorithm to solve this but went down a rabbit hole of Galois theory which wasn't required.
* Day 14: More stateful lenses.
* Day 17: Conway Cubes: More list comprehensions
* Day 18: Terrible handrolled parser for part 1. Parsec based operator parsers with associativity for part 2.
* Day 19: More parsing. Dynamic parser assembly. Nondeterministic parsing for Part 2. Most elegant solution!
* Day 20: Tile assembly. Was difficult to get right.
* Day 22: Card game - Very concise solution for part 1. Use of Sets for part 2 to speed it up.
* Day 23: Cup game - Simple solution for part 1. Very slow for part 2 - Had to find a hint and used a mutable "following cup index array" trick.
* Day 24: Hexagon grid - Main trick was translating "3d" hex coords to "2d" hex coords.
* Day 25: Just used a brute force translation of the algorithm description. Hoped to spot an obvious number-theory approach that would solve this instantly, but nothing came to mind.

## General Observations

Missing `fixf` function from stdlib.
