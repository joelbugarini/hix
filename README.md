# hix
hix active code generator written in haskell

The main idea behind this generator is to create/modify templates as easily as possible, so it can grow with your project.

Based on this lecture:
https://stackoverflow.com/questions/13666150/how-to-implement-lexical-analyser-and-parser-in-haskell
This is a work in progress, for now the Roadmap is:

* Define the grammar
* Make the Abstract Syntax tree
* Build the Parser
* Parse the config file
* Parse options from the cli
* Choose a way to read the database
* Parse the scheme
* Create utilities