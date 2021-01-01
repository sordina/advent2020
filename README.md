# Advent of Code 2020 Solutions

Various solutions to the 2020 problems. Golf style!

## Background

Original solutions were written as "oneliners" with a custom build of the
`hoe` tool. You can find these solutions in the root of this project. Since
this makes it difficult to run them yourself, I've created a cabal project to
explicitly list all the dependencies, etc.

## Source

Source for solutions is found in `src/`. With part one and two located in the same AdventDAY.hs file.

## Running

All solutions are run by calling `cabal run advent2020 DAY[b]`.

Input is provided on STDIN except in a few cases indicated by the lack of `interact` in `Main.hs`.

Example: `cabal run advent2020 22b < advent22.input`.

## Solutions of Interest

Day 1: Simple use of list-comprehensions.
Day 2: Use of lens regex library.
Day 3: Tabulated functions.
Day 6: Direct binary interpretation.
Day 7: Regexes with groups.
Day 7: Use of Zoom from Lens.
Day 10: Dynamic programming solution using Arrays for subsolutions.
Day 12: Heavy use of stateful lenses. Tabulated functions.
Day 13: Chinese remainder theoreb using `arithmoi` library.
Day 14: More stateful lenses.
