# Repository Goals

The goal of this repository is to chronicle my progress with functional<br/>
programming and the Haskell programming language.  To that end, I will<br/>
endeavor to commit code snippets to the repository as frequently as<br/>
possible, with a focus on solving targeted, manageable problems with<br/>
Haskell fundamentals.<br/>

Moreover, I will include supplemental text explaining my analysis of<br/>
the code and the problems at hand.  These projects will be contained<br/>
within directories of the form "entry_x_y," where x is the entry number<br/>
and y is a description of the problem.<br/>

I’m a functional programming novice.  Hopefully, other beginners can<br/>
learn something from my trials and tribulations, or simply laugh at the<br/>
many pratfalls that are sure to come.<br/>

# Table of Contents

## [Combinations](/entry_1_combinations/)

This directory explores combinatorics in Haskell.  It comprises the<br/>
following entries:<br/>

### [combinations_1](/entry_1_combinations/combinations_1/)

Uses list comprehensions and recursion to generate all possible<br/>
combinations of pocket twos from a standard, fifty-two card deck.<br/>

### [combinations_2](/entry_1_combinations/combinations_2/)

Uses the Map data type to generate all possible<br/>
combinations of pocket twos from a standard, fifty-two card deck.<br/>
Serves as an alternative to the recursive approach outlined in<br/>
combinations_1.<br/>

## [Poker Hands](/entry_2_poker_hands/poker_hands.md)
This directory contains three modules that may be used to accurately<br/>
describe a five-card poker hand.<br/>

### [Poker_data_types](/entry_2_poker_hands/Poker_data_types.hs)
Defines four data types - Rank, Suit, Card, and HandRank - that<br/>
are imported by Determine_hands and Describe_hands.<br/>

### [Determine_hands](/entry_2_poker_hands/Determine_hands.hs)
Defines the functions that assign a HandRank to a list of cards.<br/>

### [Describe_hands](/entry_2_poker_hands/Describe_hands.hs)
Defines the functions that return a string description of a poker hand.<br/>

## [Input and Output](/entry_3_reading_files)
This directory examines basic input and output in Haskell.  If learning<br/>
Haskell is like running a marathon, then input/output is the first of<br/>
many hills to be conquered.  Indeed, it is the first real exposure I<br/>
have had with "effectful" programming - that is, writing code that<br/>
will have an effect on the real world.<br/>

### [Reformatting a Text File](/entry_3_reading_files/Eol.hs)

Using only the Haskell prelude, Eol.hs provides the fmtFile function,<br/>
which formats a text file in accordance with user defined line breaks.<br/>
Although the text and split libraries would make this task significantly<br/>
easier, there is much to be gained by exploring alternative solutions.<br/>