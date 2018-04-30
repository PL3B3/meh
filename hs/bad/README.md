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
MetaLearning - MF(x+1) observing MF(x) learning about MF(x-1) and using those observations to improve that learning.
Programmed curiosity - Making it so there is an inherent drive for Grub to learn more things about its environment and experiment with new solutions to particualr problems
Experimentation - Grub can test hypotheses about its environment 

How?:

1 - 
    RandGen BF and MF0 code produce tuples of (MF0, original RandGen BF, and the BF String after MF modifies it)
    RandGen MF1 is trained by giving it a bunch of tuples
    RandGen MF1 gets the last two elements of a tuple and tries to figure out the MF0 code
    The generated MF0 is checked against the first element of the tuple for correctness. THis goes into a fitness score
    

2 -
    MetaFuck is applied to itself to ovserve itself learn some new task
    the MetaFuck uses this observation to mutate itself into many different forms
    Each form is tested for being more effective at learning measured in terms of quality of result as well as how quickly grub learns compared to randomness
    The concept is that the central learning capacity of grub can be applied not only to traditional takss like fitiing x's to y's but also to the task of learning itself
    Once a suitable "model" of learning is reached which can apply to itself, it can be recuirsively applied to make learning tasks faster and faster until it reaches some limit 
    But how do we reach that threshold??
    
3 - 
    
