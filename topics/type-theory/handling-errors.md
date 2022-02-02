# Handling errors

- CT v RT errors
- `error`
- `Maybe`
- `Either`
- exceptions


> One should strive to move run-time exceptions to compile-time errors.


## How to handle partial functions

* One approach is to pattern match the problematic case and use the `error` function to print the error message aborting the program.

* Where a partial is made into a total function by wrapping its result in, e.g. `Maybe`, means getting `Nothing` instead of the error message (but sometime there's nothing meaningful to do with `Nothing` but panic, possibly using the `error` function as well).

* The function declare the types they accepts and if a wrong is given right there in the source code, the compilation will fail, which is reasonable. In case of a library that's included, it also fails on compilation which is great.

* Code generated at run-time, like user input, must be sanitized, possibly re-prompting the user if he supplies the wrong type.

* The question that bothers me and prompted this concern is how to make sure that the arg to a function is a natural number i.e. positive integer? How to disallow negative numbers? Checking the input is certainly out of the question, if not for perfomance reasons, then becasue it's a RT not CT check. So, the naturals should be encoded in the type system, and still be as efficient as integers during the RT.
