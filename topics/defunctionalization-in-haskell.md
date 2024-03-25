# TPL :: Topics :: Functions :: Defunctionalization

## Defunctionalization in Haskell

In first-order programs, all functions are named and each call refers to the callee by its name.

In higher-order programs, functions may be anonymous, passed as arguments, and returned as results. As Strachey put it

Functions are:
- in a  first-order program: second-class denotable   values
- in a higher-order program:  first-class expressible values

One may then wonder how first-class functions are represented at run time.

**Closures**. First-class functions are often represented with closures. Closures are 
expressible values pairing 
a code pointer and 
the denotable values 
  of the variables 
  occurring free in that code, 
as proposed by Landin in the mid-1960's.

Today, closures are the most common representation of first-class functions in the world of *eager FP*, as well as a standard representation for implementing *OOP*. They are also used to implement higher-order logic programming.

**Defunctionalization**. Alternatively, higher-order programs can be defunctionalized into first-order programs, as proposed by Reynolds in the early 1970's.

In a defunctionalized program, 
first-class functions are represented 
with *first-order data types*: 
a first-class function 
is introduced with a constructor 
holding the values 
  of the free variables
  of a function abstraction, 
and it is eliminated 
  with a case expression 
  dispatching over the corresponding constructors.

**Combinators**. First-class functions can also be dealt with by translating functional programs into combinators and using *graph reduction*, as proposed by Turner in the mid-1970's. This implementation technique has been investigated extensively in the world of lazy FP.


We first illustrate defunctionalization with two concrete examples: In the first program, two function abstractions are instantiated once, and in the second program, one function abstraction is instantiated repeatedly.

## Program 1

Sample higher-order program with a static number of closures.

In the following ML program, `aux` is passed a first-class function, applies it to 1 and 10, and sums the results. The `main` function calls `aux` twice and multiplies the results. All in all, two function abstractions occur in this program, in `main`.

In SML

```s
(*  aux : (int -> int) -> int *)
fun aux f = f 1 + f 10

(*  main : int * int * bool -> int *)
fun main (x, y, b) = aux (fn z => x + z) *
                     aux (fn z => if b then y + z else y - z)
```

In Haskell

```hs
aux :: (Int -> Int) -> Int
aux f = f 1 + f 10

fun :: Int -> Int -> Bool -> Int
fun x y b = aux (\z -> x + z) *
            aux (\z -> if b then y + z else y - z)
```

Defunctionalizing this program amounts to defining a data type with two ctors, one for each function abstraction, and its associated `apply` function.

The first function abstraction contains one free variable (x :: Int), and therefore the first data-type ctor requires an Int.

The second function abstraction contains two free variables (y :: Int, and b :: Bool), and therefore the second data-type ctor requires an Int and Bool.

In `main`, the first first-class function is thus introduced with the first ctor and the value of x, and the second with the second ctor and the values of y and b.

In `aux`, the functional argument is passed to a second-class function `apply` that eliminates it with a case expression dispatching over the two ctors.


```hs
data Lam = Lam1 Int | Lam2 Int Bool

apply :: Lam -> Int -> Int
apply (Lam1 x) z   = x + z
apply (Lam2 y b) z = if b then y + z else y - z

aux_def :: Lam -> Int
aux_def f = apply f 1 + apply f 10

fun_def :: Int -> Int -> Bool -> Int
fun_def x y b = aux_def (Lam1 x) * aux_def (Lam2 y b)
```

In SML

```s
datatype lam = LAM1 of int
             | LAM2 of int * bool

(* apply : lam * int -> int *)
fun apply (LAM1 x, z)
      = x + z
    | apply (LAM2 (y, b), z)
      = if b then y + z else y - z

(* aux_def : lam -> int *)
fun aux_def f
    = apply (f, 1) + apply (f, 10)

(* main_def : int * int * bool -> int *)
fun main_def (x, y, b)
    = aux_def (LAM1 x) * aux_def (LAM2 (y, b))
```
