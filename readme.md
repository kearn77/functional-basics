# Repository Goals

The goal of this repository is to chronicle my progress with functional 
programming and the Haskell programming language.  To that end, I will 
endeavor to commit code snippets to the repository as frequently as possible, 
with a focus on solving targeted, manageable problems with Haskell 
fundamentals.  

Moreover, I will include supplemental text explaining my analysis of the code 
and the problems at hand.  These projects will be contained within directories 
of the form "entry_x_y," where x is the entry number and y is a description of 
the problem.

I’m a functional programming novice.  Hopefully, other beginners can learn 
something from my trials and tribulations, or simply laugh at the many 
pratfalls that are sure to come.

# Table of Contents

## [Combinations](/entry_one_combinations/)

This directory explores combinatorics in Haskell.  It comprises the following
entries:

- [combinations_1](/entry_one_combinations/combinations_1/)

Uses list comprehensions and recursion to generate all possible combinations
of pocket twos from a standard, fifty-two card deck.

- [combinations_2](/entry_one_combinations/combinations_2/)

Uses higher order functions and the Map data type to generate all possible
combinations of pocket twos from a standard, fifty-two card deck.  Serves as
an alternative to the recursive approach outlined in combinations_1.