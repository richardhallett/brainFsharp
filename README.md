# brainFsharp
A brainfuck interpreter written in F#

This was an experiment in writing a parser/intepreter for the instruction set in F#, brainfuck despite it's expletive name 
is a pretty good language for this.

Steps:
1. Construct a basic AST from the instruction set
2. Optimise the AST so it's not quite as slow
3. Run through all the commands recursively performing appropriate actions

I built it on top of dotnet core so if you want to run it just ensure you have both dotnet and the F# SDK and do a *dotnet run*

It will generate you a lovely mandlebrot by default, but the real beauty is the code.
![alt text](https://raw.githubusercontent.com/richardhallett/brainFSharp/master/brainfsharp-mbrun.gif "Example")
