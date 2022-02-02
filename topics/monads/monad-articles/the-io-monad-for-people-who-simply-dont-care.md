---
downloaded:       2022-01-03
page-url:         http://blog.sigfpe.com/2007/11/io-monad-for-people-who-simply-dont.html
page-title:       A Neighborhood of Infinity: The IO Monad for People who Simply Don't Care
article-title:    The IO Monad for People who Simply Don't Care
---
# A Neighborhood of Infinity: The IO Monad for People who Simply Don't Care

The theoretical underpinnings of monads in Haskell can be tricky to grasp. If you simply want to read some input and write some output, and you don't want to waste time with nonsense like the "monad laws", you've come to the right place.
The theoretical underpinnings of monads in Haskell can be tricky to grasp. If you simply want to read some input and write some output, and you don't want to waste time with nonsense like the "monad laws", you've come to the right place.

### Getting Started

Many programming languages make a distinction between expressions and commands. For example, in BASIC you couldn't say

LET A = PRINT 1

and in C you can't say

x = if (a==1) { 2; } else { 3; }

. Haskell is no different. Like other languages it makes the distinction, and like other languages it has its own slightly idiosyncratic notion of what the difference is. The IO monad is just a device to help make this distinction. And that's the last time I'll mention the word 'monad' for a while.

Here's an example of a Haskell function:

f x = 2\*x+1

The right hand side is an expression. Expressions are made of subexpressions and the value of an expression is determined by the value of its subexpressions. There is no room for anything like a

print

command here because a print command doesn't return a value, it produces output as a side-effect. So in order to do I/O we need something different: the

do

keyword. It's easy to use: you just write

do

and then follow it by an indented list of commands. Here's a complete Haskell program:

main = do  
    print "Hello"  
    let x = 1+2+3+4+5  
    print "The answer is"  
    print (2\*x)

Note the

let

. It's just like the old days of BASIC programming where you can use

let

to assign a name to a value. Note also that commands take arguments that can be expressions. So

print (2\*x)

is a command, but

2\*x

is an expression. Again, little different from a language like Python.

But what about input? Haskell has a command called

getLine

that reads input from the user. So here's an interesting feature of Haskell: commands can return values. But a command that returns a value is different from an expression with a value. The difference means we have to write code like this:

main = do  
    print "Hello"  
    x <- getLine  
    print "The input was"  
    print x

We have to use

<-

instead of

let ... = ...

to get a returned value out of a command. It's a pretty simple rule, the only hassle is you have to remember what's a command and what's a function.

You can also write your own commands. Here's a simple example that returns a pair of lines entered by the user:

get2Lines = do  
    line1 <- getLine  
    line2 <- getLine  
    return (line1,line2)

main = do  
    (a,b) <- get2Lines  
    print ("You typed " ++ a ++ " then " ++ b ++ ".")

Note how we used

<-

instead of

let

to get the result of

getLine

. We also did the same to get the result of

get2Lines

. But notice one extra feature: we had to use

return

to return a value out of a

do

block.

So that's pretty much it. Here are the rules:

  
2.  To introduce a list of commands, use do.  
    
3.  To return a value from a command, use return.  
    
4.  To assign a name to an expression inside a do block use let.  
    
5.  To assign a name to the result of a command, use <-.  
    

So how do you know what's a command and what's an expression? If it has any chance of doing I/O it must be a command, otherwise it's probably an expression. But you can do silly things like:

  
f x = do  
    return \[x,x\]

main = do  
    x <- f 10  
    print x

so occasionally there will be commands that don't do any IO. But not the other way round.

### Types

Everything in Haskell has a type, even commands. In general, if a command returns a value of type

a

then the command itself is of type

IO a

. It just has an

IO

wrapped around the return type. For example,

getLine

is of type

IO String

and

f

above is of type

a -> IO \[a\]

. Apart from that extra

IO

it's like ordinary expression types. But note that if you have a bit of code like

main = do  
    print (getLine)

you get a type error. This is because you must use

<-

to extract the input string from the

getLine

command. Because

getLine

has type

IO String

instead of type

String

the compiler can catch these kinds of mistakes. Giving commands and expressions different types means we can't accidentally mix them up.

At this point you might have realised that if

f x = do  
    return x

main = do  
    x <- f 5  
    print x

is legal, then this might be legal too:

main = do  
    x <- return 1  
    print x

It is.

return

is simply a function of type

a -> IO a

that converts a value of type

a

to a command of type

IO a

.

You can also rewrite this

inputWithPrompt = do  
    print "Hello, enter a line"  
    x <- getLine  
    return x

with

inputWithPrompt = do  
    print "Hello, enter a line"  
    getLine

In the first version we extracted

x

from

getLine

and then converted this value back into another command with

return

. In the second version we took a shortcut and simply ended with

getLine

. So we probably ought to add one more rule

  
5\. The type of a sequence of commands is the type of the last line.  

  
getLine

already has the type of a command that returns a string so we can just make it the last line. But use the former version if you find this confusing, and then you only need four rules.

There's one last thing that confuses people. The type of an

if

statement is the type of its branches. So if you want an

if

statement inside a

do

block, it needs to be a command, and so its branches need to be commands also. So it's

main = do  
    if 2/=1  
        then do  
            print "2/=1"  
            print "Cool"  
        else do  
            print "2==1"  
            print "Not cool"

not

main = do  
    if 2/=1  
        then  
            print "2/=1"  
            print "Cool"  
        else  
            print "2==1"  
            print "Not cool"

Oh and a little tip. If the

do

block has just one command, you can drop the

do

. So this compiles:

main = do  
    if 2/=1  
        then  
            print "2/=1"  
        else  
            print "2==1"

### Why make the command/expression distinction at all?

You don't care, or you wouldn't be reading this. But suppose you did. Then I'd say: Firstly, let's state clearly what this distinction is. If something is of type

IO a

then it's a command returning an value of type

a

. Otherwise it's an expression. That's the rule. So here's the important bit: following the rules above it's completely impossible to put an executed command inside an expression. As the only way to do I/O is with commands, that means you have no way to find out what Haskell is doing inside expressions. And that's the whole point! Haskell likes to keep its evaluation secret from you and doesn't want you outputting information that might give away what's going on. For example, if

(print 1)+(print 2)

were legal Haskell, and it printed 1, followed by 2, then you'd know that it evaluated the left argument to

+

first, and then the second. Haskell doesn't want to reveal this because it wants the freedom to rearrange your code internally without changing its behaviour. And this makes things easier for you too. When you see

a+b

you know its exactly the same as

b+a

, whereas in C++, say, you have to worry about whether

a

and

b

have side effects. If the type isn't

IO a

, then you can sleep at night in the confidence that there are no side effects.

(Do you see why you can't sneak a command into an expression? It's quite neat. To make an expression out of a command you'd need to do something like extract a return value out of a command. But to do that you need to use

<-

to extract the value of the command. But you can only use

<-

inside a

do

block. So the result has to be a command. Once you're in a command, there's no way out!)

### It's all lies I tell you!

One last thing. Much of what I've said above is false. But what I hope I have done is describe a subset of Haskell in which you can start some I/O programming without having a clue what a monad is. Once you're successfully bootstrapped, then you'll be ready for [the truth][1]. No more of that "monads are too hard" defense.

PS You can think of the above as using [taint][2] as a way to sequester commands from other expressions.

Back to E8 this weekend...

[1]: http://www.disi.unige.it/person/MoggiE/ftp/ic91.pdf
[2]: http://sigfpe.blogspot.com/2007/04/trivial-monad.html
