I have a few ideas for the grub:

First, defining some things:

BF: Brainfuck, an esoteric programming language consisting of 8 symbols. Turing complete by the way. This modifies a numerical Tape of infinite cells with any int value
MF: MetaFuck, Brainfuck which modifies brainfuck and can apply to itself - Also Turing complete. Runs at many levels. MF0 modified BF, MF1 modified MF0, etc.
RandGen: Randomly generated Code out of 8 symbols.
Grub: A placeholder name for the collective metafuck and brainfuck code and abstractions produced by the program

Some Possible Goals:

One single String of Metafuck which applies to itself and some string of input (possibly brainfuck or Metafuck one level below it) recursively to achieve some goal
A process of some sorts by which the Grub can create better and better learning algorithms which allow it to confront some new problem area (like, for example, trying to fit a linear model to an input and output tape of ints) and achieve a solution in BF in signifcantlt less time than it would take for RandGen
Grub learning about the function of each symbol in order to combine them into functions to solve problems
