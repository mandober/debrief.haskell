# Functional Programming with Haskell

https://www.mta.ca/~rrosebru/oldcourse/371199/haskell/paper.htm

> Functional Programming

Functional Programming

Using _Haskell_

Wade Estabrooks

Michael Goit

Mark Steeves

Table of Contents

[1\. Introduction to Haskell](#intro)

[2\. Evaluation of the Language](#eval)

[2.1 Readability](#read)

[2.2 Writability](#write)

[2.3 Reliability](#relia)

[2.4 Cost](#cost)

[3\. Our Program](#prog)

Appendix: Source Code

[Appendix A](#appenda)\- source code for the Dictionary module.

[Appendix B](#appendb)\- source code for the Words module.

[Appendix C](#appendc)\- source code for the Fixwords module.

1\. Introduction to Haskell

Haskell is a general purpose, purely functional programming language incorporating many recent innovations in programming language design. Haskell provides higher-order functions, non-strict semantics, static polymorphic typing, user-defined algebraic datatypes, pattern-matching, list comprehensions, a module system, a monadic I/O system, and a rich set of primitive datatypes, including lists, arrays, arbitrary and fixed precision integers, and floating-point numbers. Haskell is both the culmination and solidification of many years of research on lazy functional languages.

True, this definition is a bit long, but it almost completely shows the power that Haskell has as both a functional language, and as a programming language in general. Because Haskell is a purely functional language is has certain characteristics, and also because Haskell is modern language, many historical mistakes made with language design have been avoided. Recent advances in Typed-Lambda Calculus have formed a basis for Haskell, and the addition of very strong typing to a functional language makes Haskell (unlike LISP) "safe" to use. The typing rules also allow a program Haskell to be validated.

One might wonder what it means to be purely functional, and the simple answer is that a purely functional language has these characteristics. A program, or a program segment is well behaved, meaning that it has no side-effects. There are no looping, or other control structures at all; all "looping" is done through recursion. There are no variables, Haskell uses one-time assignment, where a function or a program receives a value once, and that value does no change throughout the course of it's life.

Lexical scoping, where the scope of an identifier is fixed at compile-time to be the smallest block (function/procedure body) containing the identifier's declaration. This means that an identifier declared in some block is only accessible within that block and from procedures declared within it.

Functional programming languages are based on lambda-calculus. The lambda-calculus grew out of an attempt by Alonzo Church and Stephen Kleene in the early 1930s to formalize the notion of computability. It is a formalization of the notion of functions as rules (as opposed to functions as tuples). As with mathematical expressions, it is characterized by the principle that the value of an expression depends only on the values of its sub-expressions. The lambda-calculus is a simple language with few constructs and simple semantics; but, it is expressive enough to express all computable functions.

The special character of the lambda-calculus is illustrated when it is recognized that functions may be applied to other functions and even permit self application. It is also important to note that pure lambda-calculus does not have any built-in functions or constants, even numbers are represented as functions. Typically, practical languages extend the lambda-calculus with common mathematical operations and constants and also drop some of the parentheses to improve the readability of the lambda expressions. A lambda-expression is executed by evaluating it; evaluation proceeds by repeatedly selecting a reducible expression and reducing it, this is called \\beta-reduction.

Typed lambda-calculus is a variety of lambda-calculus in which every term is labelled with a type. A function application f g is only syntactically valid if f has type s -> t, where the type of g is s (or an instance of s in a polymorphic language) and t is any type. If the types allowed for terms are restricted, then no term may be applied to itself, thus avoiding one kind of non-terminating evaluation. Haskell is actually based on Polymorphic lambda-calculus; which is an extension of typed lambda-calculus allowing functions which take types as parameters. Polymorphic lambda-calculus was invented by Jean-Yves Girard in 1971 and independently by John C. Reynolds in 1974. (Skibinski)

This was just a quick introduction to lambda-calculus; to give you a taste of an underlying mechanism under the hood of Haskell. The only practical form of lambda-calculus that appears in Haskell are anonymous functions, such as \\x->x\*x+3\*x. But even this form can be replaced by a sugared version of lambda expression.

A program in Haskell (or any purely functional language for that matter) consists of a set of possibly recursive function definitions and an expression whose value is output as the program's result. Furthermore, there are no side-effects to expression evaluation so an expression will always evaluate to the same value. The order of evaluation of sub-expressions is determined by the language's evaluation strategy. In a strict (call-by-value) language this will specify that arguments are evaluated before applying a function whereas in a non-strict (call-by-name) language arguments are passed unevaluated. Haskell uses call-by-name evaluation, or lazy-evaluation to evaluate expressions. Both topics are beyond the scope of this paper.

Haskell supports the use of a lot of abstraction, which is very useful especially with large problems. There is a wide array of forms that abstraction can take, through lambda notation in an expression (as we have seen); through the use of modules, or separate program files. Haskell can also use the idea of Algebraic Data Types using pattern matching, and Abstract Data Types's through modules and classes, and both single and multiple inheritance. All Haskell functions, definitions and types are first class, meaning that they can be passed to functions.

  

2\. Evaluation of the Language

2.1 Readability

Without any control structures, programs written in Haskell have to be broken up into many small pieces and they have to use a lot of recursion. This makes code written in Haskell very readable; the smaller pieces makes it easy to trace a program. The use of recursion is sometimes a little difficult to trace, this isn't really an issue with the language since recursion is difficult in any language; rather it is an issue with, but with a little thought it easy to understand what is going on.

Haskell allows for long identifiers, and the use of underscores to split up words in identifiers. Any function can be defined to be infix, and along with long identifiers, this can makes program expressions read more like sentences. Lambda abstractions, which are extensively used in Haskell, make an expression more general. This generality adds continuity between expressions and augments cognition when reading a program, or a function.

Since Haskell supports user defined data types and modules, the programmer can make it as readable or reusable as he/she wants. Remember, "Higher-order + Polymorphic = Reusable" according to Simon Thompson.

2.2 Writability

Haskell's strong typing eliminates many type compatibility errors. Strong typing means the language does not have a lot of (possibly non-orthagonal) typing rules for the programmer to remember. Polymorphic type system enhances code re-usability; a function can be written to work on any given data type where it is not dependent on a characteristic of that type. For example, Haskell lets you define a function to have the type a, where a is filled in at compile time (or run-time if it's interpreted).

_getListSize :: \[a\] -> Integer_

_getListSize \[\] = 0_

_getListSize (a:as) = 1 + getListSize as_

In this example getListSize determines the number of elements in a list of any type. Counting the number of elements in a list is not dependent on the type of the list elements, so this is a good application of polymorphism.

The programmer does not need to manage storage. Memory allocation and initialization is implicit, and deallocation is automatically performed by garbage collection.

Recursion is sometimes helpful to writability provided the programmer is able to think recursively. On the other hand there are practical issues with recursion when it is too deep. There is no other looping mechanism, and recursion over, say, a million-item list will undoubtedly be problematic.

Monads (as used in monadic I/O) are so abstract, that they are difficult to use. However, for simple programming tasks, monads can be ignored. This is discussed in some detail in the "Our Program" section of this paper.

2.3 Reliability

Any purely functional language is, by the nature of the functional programming paradigm, very reliable and easy to verify. With Haskell's strong typing approach, it is no exception and is very reliable, but as we will discuss, there is one shortcoming.

There is no such thing as type coercion, and the typing rules are very strict. For example, to add an integer to a float, you must call a function to convert the integer to a float, then you can add the two numbers. Type checking occurs at both compile time and run time, and the program will not compile or run if there are any typing errors. This makes it impossible to have type incompatibilities, which makes programs very reliable.

Haskell does not allow any functions to have any side effects; which means that the only variables that the function can change are local to the function. With Haskell's pass-by-name convention, parameters passed to functions can never be changed and are only evaluated when the value of the parameter is needed. The ability to force functions and programs to not affect the state of any other program or function is huge asset to the reliability of Haskell.

It seems too good to be true, and like everything that seems this way, there is one shortcoming to Haskell's reliability. As we have seen, since Haskell is purely functional there are no control structures, so anything useful has to be done with recursive functions. The problem with this is that when a function is recursively called, the stack is used to store any information needed by that function, and if the recursive function calls itself often enough, the stack will run out of memory. This seriously limits the size of any data structures that are manipulated using recursion; and this really limits the usefulness of Haskell in any large or "real-world" applications.

2.4 Cost

Haskell is a completely free language to use. The interpreter and compilers are available as freeware, and are available for the DOS, Windows 3.x, 95, 98, NT, and Unix platforms. There are no program release restrictions, which means that you can sell anything that you program. There are quite a few libraries available freely over the Internet, and Haskell is completely open source.

Training people to use Haskell can be costly, especially if the people are used to programming with an imperative language. It is difficult to learn a functional language, including Haskell after learning imperative, the thought process involved can quite different. Although, since Haskell is so well defined, and is a modern language, with a little practice it really isn't too bad.

The space requirements for Haskell are small, the interpreter (including library files) is around 4 Mb. As mentioned in the reliability section, the memory requirements for the stack can be extensive if a function is recursively called often enough. This stems partly from the nature of functional programming, and partly from the nature of an interpreted program.

3\. Our Program

For our program, we decided to do a dictionary. This program checks an ASCII text file against a dictionary of acceptable words. To accomplish this we used one main data structure to store words, a list. The list was used to store the dictionary and the file that is to be checked.

We defined a new type for our program, called a word. Here is the type definition of the word: "_type Word = \[Char\]_". The dictionary and the file that is to be checked are list of this Word type. The dictionary was sorted, to make the search for words faster.

To check to see if a word is in the dictionary, we would simply check to see if that word was an element of the dictionary list. We do this by using the built in function _\`elem\`_. If the word is not in the dictionary list, then we would ask the user if they wanted the word added to the dictionary. If the user wants the word added, then the dictionary was updated, and re-sorted.

When trying to incorporate input/output into the dictionary, we ran into a few problems. Basically, the problem was that when a character is read from the keyboard, it has the type _IO Char_. This type is type incompatible with _Char_. So simple interaction with the user became almost impossible. With Haskell, as was mentioned before, functions cannot have any side effects. Since I/O is by nature side effects, Haskell handles it differently. Monadic I/O is used to overcome this problem. Since, in our opinion, monads are beyond the scope of this project we will not go into any detail, except to say that Haskell's I/O is best described by Jan Skibinski, who said, "It is no more necessary to understand monad theory to perform Haskell I/O than it is to understand group theory to do simple arithmetic."

After overcoming Haskell's I/O, there were no more setbacks in writing the program. When we had finished the program, and started testing, we noticed a little problem. Once our dictionary got to about 4000 words, we started running out of stack when reading the file. The cause of our stack overflow problem is that when the program reads the dictionary file, and separates it into words, it does it recursively (_splitWords_ function). Every time that the splitWords function recursively calls itself, another word is added to the stack, so the stack is quickly used up. To overcome this problem, we limited the dictionary file to around 3000 words.

The program is split into three modules, these are found in the appendices.

Bibliography

Jan Skibinski, The Haskell Companion.

[http://www.numeric-quest.com/haskell/hcompanion/](http://www.numeric-quest.com/haskell/hcompanion/)

Simon Peyton Jones (editor), The Haskell 98 Report

[http://www.haskell.org/onlinereport/](http://www.haskell.org/onlinereport/)

Paul Hudak, John Peterson and Joseph Fasel, A Gentle Introduction to Haskell

[http://haskell.cs.yale.edu/tutorial/](http://haskell.cs.yale.edu/tutorial/)

Hamilton Richards, CS307 Lecture notes, Utexas at Austin

[http://www.cs.utexas.edu/users/ham/UTCS/CS307/](http://www.cs.utexas.edu/users/ham/UTCS/CS307/)

Appendix A

\--DICTIONARY--

\--NEED TO HAVE A FILE CALLED "dict.txt" DEFINED IN THE LOCAL DIRECTORY.

\--NOVEMBER 21, 1999

\--dict.hs-------------------------------------------------------------

module Dictionary where

import Prelude

import IO

import List

import "words"

import "fixwords"

dictFile = "dict.txt" --This is the file name of the dictionary file.

main = getFileName >>= \\file ->

loadDict dictFile >>= \\dict ->

sortDict dict >>= \\dict ->

sortDict dict >>= \\dict ->

checkFile file dict >>= \\dict ->

saveDict dict >>

putStr"\\nThank you for using this dictionary."

\--Loads a text file f and checks if its words are in the dictionary d.

checkFile :: String->\[Word\]->IO \[Word\]

checkFile f d = do{

ck<-loadDict f;

d<-checkAll ck d;

return d;

}

\--Checks if words in the given list are in the dictionary d.

\--When a word is not in d, the user is asked if the word should be added.

\--The dictionary is returned with any new words added.

checkAll :: \[Word\]->\[Word\]->IO \[Word\]

checkAll \[\] d = return (d) --If the wordlist is empty, then return.

checkAll (w:ws) d

|null w = checkAll ws d --fixWord can make given word \[\]

|w \`elem\` d = checkAll ws d --if w in d, check the rest

|otherwise = do{

d<-questionWord w d;

checkAll ws d; --Gotta love recursion!!!

}

\--Asks the user if the word w is to be added to the dictionary d.

\--If the user answers 'y', w is added to d.

\--Takes a single word as the first parameter, and the dictionary

\--as the second. The dictionary is returned with any new words

\--added, and sorted.

questionWord :: Word->\[Word\]->IO \[Word\]

questionWord w d = do{

putStr ("\\n '"++w++"' is not in the dictionary! Add it? (y/n) ->");

getChar>>= (\\y\_n->return(addWord y\_n w d))

}

\--If c is the character 'y' or 'Y', adds word w to dictionary d and returns

\--d after sorting. Otherwise, just returns d.

addWord ::Ord a => Char -> a -> \[a\] -> \[a\]

addWord c w d

|c=='Y'||c=='y' = sort(d++\[w\])

|otherwise = d

\--Sorts the dictionary

sortDict ::(Monad a, Ord b) => \[b\] -> a \[b\]

sortDict d = return (sort d)

\--Saves the dictionary

saveDict :: \[Word\]->IO()

saveDict d = openFile dictFile WriteMode>>=

(\\handle->hPutStr handle (unwords d))

\--Reads the dictionary file.

\--loadDict ::String->IO \[Word\]

loadDict file = openFile file ReadMode>>= (\\handle->hGetContents handle >>=

(\\line->return(splitWords line)))

\--This long line uses lamba abtractions to open a file, read its contents,

\--and split the contents into words. There are basically three parts to

\--this line:

\--"openFile file ReadMode"

\-- This opens a file for read mode, and creates a handle.

\--"\\handle->hGetContents handle"

\-- The variable handle is defined by the previous function call

\-- (openFile). hGetContents takes a file handle as a parameter

\-- and returns a string containing all of the characters in the file.

\--"\\line->return(splitWords line)"

\-- The variable line is what is returned from hGetContents.

\-- The rest of this part just returns the \[word\] list by

\-- making a call to splitWords with the whole file.

\--Returns a file name entered by the user.

getFileName ::IO String

getFileName = do {

putStr "Check what file: ";

line<-getLine;

return line;

}

Appendix B

\--words.hs------------------------------------------------------------

\--These functions deal with splitting strings into lists of words.

module Words where

import "fixwords" --definition for type Word; function fixWord

ignore = "\\n \\t" --Definition of the whitespace that we ignore

\--Takes a String and splits into a list of type Word

splitWords :: String -> \[Word\]

splitWords \[\] = \[\]

splitWords text = fixWord(getWord(dropSpace text)) :

splitWords (dropSpace (dropWord text))

\--Gets the first word from a String

getWord ::String->Word

getWord \[\] = \[\] -- end of string encountered

getWord (c:cs)

|c \`elem\` ignore = \[\] -- hit whitespace

| otherwise = c: getWord cs

\--Removes the first word from the given String

dropWord ::String->String

dropWord \[\] = \[\]

dropWord (c:cs)

|c \`elem\` ignore = (c:cs)

|otherwise = dropWord cs

\--Removes leading whitespace (elements of ignore) from a String

dropSpace ::String->String

dropSpace \[\] = \[\]

dropSpace (c:cs)

|c \`elem\` ignore = dropSpace cs

|otherwise = c:cs

Appendix C

\--fixwords.hs---------------------------------------------------------

\--These functions make the word pretty so the dictionary won't have

\--multiple entries for a word with differences in only case or

\--neighboring punctuation.

module Fixwords where

import Prelude --include definition for Char, isAlpha, toLower

type Word = \[Char\] -- a Word is a list of Char

\--Given a Word, strips leading and trailing non-alphabetic characters

\--(such characters in the middle of a word are not removed), and converts

\--it to lower case.

fixWord :: Word->Word

fixWord \[\] = \[\]

fixWord (w:ws) = lowerCase(stripTP(stripLP(w:ws)))

\--Accepts a Word as a parameter, strips leading punctuation, and

\--returns the new Word.

stripLP :: Word->Word

stripLP \[\] = \[\]

stripLP (w:ws)

\--If the first Char is a letter, retval is same as param;

|(isAlpha w) = w:ws

\--otherwise, return result of stripLP with the first

\--Char removed.

|otherwise = stripLP ws

\--Accepts a Word as a parameter, strips trailing punctuation (i.e. non-

\--alphabetic characters), and returns the new Word.

stripTP :: Word->Word

stripTP \[\] = \[\]

stripTP (w:ws)

\--If the last Char is a letter, retval is same as param;

|(isAlpha (last (w:ws))) = w:ws

\--otherwise, take last Char off, and return stripTP(that)

\--provided there is at least one Char in Word.

\--The condition "null xs" happens after all characters have

\--been stripped (e.g., everything comes off "123").

|otherwise = if null ws then \[\] else stripTP (w:init ws)

\--Accepts a Word as a parameter, converts each Char to lower case,

\--and returns the converted Word.

lowerCase :: Word->Word

lowerCase \[\] = \[\]

lowerCase (w:ws) = (toLower w):(lowerCase ws)


[Source](https://www.mta.ca/~rrosebru/oldcourse/371199/haskell/paper.htm)