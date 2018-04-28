# meh
This program has a function F.

that function takes an input array X  and an output array Y, where X and Y are signed single byte integers.
It returns a [BrainF***](https://esolangs.org/wiki/Brainfuck) program that, if run on a tape with an initial state of X, will modify the tape to set it to a final state of Y.

for example:

```
brainF = f(x,y1)

y2 = RunBFCode(code=brainF,
               initialTape=X)

//y1=y2
```

What makes f interesting is that you can call it with it's own result.
To demonstrate how this works, here's an example of BrainF*** code: 
```
++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.
```
You could also represent this string as an array of chars, or even an array of numbers:

```
[0,0,0,0,0,0,0,0,1,2,0,0,0,0,1,2,0,0,2,0,0,0,2,0,0,0,2,0,3,3,3,3,4,5,2,0,2,0,2,4,2,2,0,1,3,5,3,4,5,2,2,6,2,4,4,4,6,0,0,0,0,0,0,0,6,6,0,0,0,6,2,2,6,3,4,6,3,6,0,0,0,6,4,4,4,4,4,4,6,4,4,4,4,4,4,4,4,6,2,2,0,6,2,0,0,6]
```

Because F returns a program that convers one array of integers into another, one can do the following

```
InitialX = ...
InitialY = ...
X = InitialX
Y = InitialY

repeat 10 times:
  BF_Code = f(X,Y)
  Y = StringToArray(BF_Code)
```

At the end, you can 
set a tape to state X,
run BF_Code,
set BF_Code to the output, and repeat the loop untill you get Y. 

<!-- from meh.hs -->
<!-- Q!, 10-5-17
We desire insight on desire.
End goal is to formulate a map of what people want. Very difficult.
What people want is manifested in  -->
