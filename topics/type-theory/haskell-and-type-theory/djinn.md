# Djinn

Djinn by Lennart Augustsson

https://web.archive.org/web/20210423032732/http://lambda-the-ultimate.org/node/1178

Djinn is a program that generates Haskell code from a type, relying on the Curry-Howard isomorphism, intuitionistic logic, intuitionistic propositional calculus, Gentzen sequent calculus, Gentzen's LJ sequent calculus.

Given a type, Djinn will deduce an expression of that type, if it can found one. If Djinn says the type is not realizable it means it cannot find a (total) expression of the given type.

Djinn only knows about tuples, `->`, and the data types in the initial environment (type `:e` to the initial environment).

Even if it comes up with solutions, the expressions may have the right type, but they may not be semantically correct.

## Theory

Djinn interprets a Haskell type as a logic formula using the *Curry-Howard isomorphism* and then uses a decision procedure for *Intuitionistic Propositional Calculus*.

This decision procedure is based on *Gentzen's LJ sequent calculus*, but in a modified form, *LJT*, that ensures termination. This variation on LJ has a long history, but the particular formulation used in Djinn is due to Roy Dyckhoff, which he described in his paper:

* `Contraction-free sequent calculi for intuitionistic logic`   
  Roy Dyckhoff, 1992
  http://www.dcs.st-and.ac.uk/~rd/publications/jsl57.pdf

* `Contraction-free sequent calculi for intuitionistic logic: A correction`   
  Roy Dyckhoff, 2018

The decision procedure has been extended to generate a proof object as a lambda term. It is this lambda term, in the normal form, that constitutes the Haskell code.

Since Djinn handles propositional calculus it also knows about the absurd proposition, corresponding to the empty set. This set is sometimes called `Void` in Haskell, and Djinn assumes an elimination rule for the Void type:

`void :: Void -> a`

Using `Void` is of little use for programming, but can be interesting for theorem proving. For example, the double negation of the law of excluded middle:

```hs
Djinn> type Not x = x -> Void
Djinn> f ? Not (Not (Either x (Not x)))
f :: Not (Not (Either x (Not x)))
f a = void (a (Right (\ b -> a (Left b))))
```

where the `Not` type was defined as `type Not x = x -> Void`

Of course, the regular version of the law of excluded middle cannot be proven:

```hs
Djinn> f ? Either x (Not x)
-- f cannot be realized.
```

## Initial environment

```hs
Djinn> :e
data () = ()
data Either a b = Left a | Right b
data Maybe a = Nothing | Just a
data Bool = False | True
data Void
type Not x = x -> Void
class Monad m where return :: a -> m a; (>>=) :: m a -> (a -> m b) -> m b
class Eq a where (==) :: a -> a -> Bool
```

## Commands

Commands may be abbreviated:

```
:clear                                Clear the envirnment
:quit                                 Quit program
:environment                          Show environment
:delete <sym>                         Delete from environment
:load <file>                          Load a file with djinn commands

type  <sym> <vars> = <type>           Add a type synonym
data  <sym> <vars> = <datatype>       Add a data type
class <sym> <vars> where <methods>    Add a class
<sym> :: <type>                       Add to environment

? <sym> :: <type>                     Query
<sym> ? <type>                        Query
?instance <sym> <types>               Query instance

:help                                 Print help
:verboseHelp                          Print verbose help

:set <option>                         Set options

Current options:
  -multi      print multiple solutions
  +sorted     sort solutions
  -debug      debug mode
  cutoff=200  maximum number of solutions generated
```


## Djinn commands

1. <sym> ? <type>

*QUERY*: Try to find a function of the specified type. Djinn knows about the function type, tuples, `Either`, `Maybe`, `()`, `Void`, and can be given new type. Additional functions, type synonyms, and data types can be added by using the commands below. If a function can be found it is printed in a style suitable for inclusion in a Haskell program. If no function can be found this will be reported as well. Examples:

```hs
Djinn> f ? a->a
f :: a -> a
f a = a

Djinn> sel ? ((a,b),(c,d)) -> (b,c)
sel :: ((a, b), (c, d)) -> (b, c)
sel ((_, a), (b, _)) = (a, b)

Djinn> cast ? a->b
-- cast cannot be realized.
```

Djinn will always find a (total) function if one exists. The worst case complexity is bad, but unlikely for typical examples. If no function exists Djinn will always terminate and say so. When multiple implementations of the type exists Djinn will only give one of them. Example:

```hs
Djinn> f ? a -> a -> a
f :: a -> a -> a
f _ a = a
```


2. <sym> :: <type>

*Add* a new function available for Djinn to construct the result. Example:

```hs
Djinn> foo :: Int -> Char
Djinn> bar :: Char -> Bool
Djinn> f ? Int -> Bool
f :: Int -> Bool
f a = bar (foo a)
```

This feature is not as powerful as it first might seem. Djinn does *not* instantiate polymorphic functions. It will only use the function with exactly the given type. Example:

```hs
Djinn> cast :: a -> b
Djinn> f ? c->d
-- f cannot be realized.
```


3. type <sym> <vars> = <type>

*Add* a Haskell style type synonym. Type synonyms are expanded before Djinn starts looking for a realization. Example:

```hs
Djinn> type Id a = a->a
Djinn> f ? Id a
f :: Id a
f a = a
```

4. type <sym> :: <kind>

*Add* an abstract (uninterpreted) type of the given type. An uninterpreted type behaves like a type variable during deduction.

5. data <sym> <vars> = <type>

*Add* a Haskell style data type. Example:

```hs
Djinn> data Foo a = C a a a
Djinn> f ? a -> Foo a
f :: a -> Foo a
f a = C a a a
```

6. data <sym> <vars>

*Add* an empty type.

7. class <sym> <vars> where <methods>

*Add* a type class.

```hs
class Ord a where compare :: a -> a -> Ordering
```

8. :delete <sym>

Remove a symbol that has been added with the `:add` command.

9. `:load <file>`

Read and execute a file (Haskell comments allowed) with djinn commands.

10. `:set` Set runtime options.

* `+multi` show multiple solutions, but not all since may be infinitly many.
* `-multi` show one solution
* `+sorted` sort solutions according to a heuristic criterion
* `-sorted` do not sort solutions
* `cutoff=N` compute at most N solutions

The heuristic used to sort the solutions is that as many of the bound variables as possible should be used and that the functionshould be as short as possible.

12. `:environment`  List all added symbols and their types.
11. `:clear`        Set the environment to the start environment.
13. `:verbose-help` Print this message.
14. `:quit`         Quit Djinn.



## More examples

```hs
calvin% djinn
Welcome to Djinn version 2011-07-23
Type :h to get help.


-- II continuation monad (return, bind, callCC)

-- 0) the only new thing to add to env:
Djinn> data CD r a = CD ((a -> r) -> r)

-- 1) then query 'return'
Djinn> returnCD ? a -> CD r a
returnCD :: a -> CD r a
returnCD a = CD (\ b -> b a)

-- 2) then query '>>='
Djinn> bindCD ? CD r a -> (a -> CD r b) -> CD r b
bindCD :: CD r a -> (a -> CD r b) -> CD r b
bindCD a b =
      case a of
      CD c -> CD (\ d ->
                  c (\ e ->
                    case b e of
                    CD f -> f d))

-- 3) then query 'callCC'
Djinn> callCCD ? ((a -> CD r b) -> CD r a) -> CD r a
callCCD :: ((a -> CD r b) -> CD r a) -> CD r a
callCCD a =
      CD (\ b ->
          case a (\ c -> CD (\ _ -> b c)) of
          CD d -> d b)


-- I state monad (return, bind)

Djinn> type S s a = (s -> (a, s))

Djinn> returnS ? a -> S s a
returnS :: a -> S s a
returnS a b = (a, b)

Djinn> bindS ? S s a -> (a -> S s b) -> S s b
bindS :: S s a -> (a -> S s b) -> S s b
bindS a b c =
    case a c of
    (d, e) -> b d e


-- a function type may have a type class context:

Djinn> refl ? (Eq a) => a -> Bool
refl :: (Eq a) => a -> Bool
refl a = a == a
```

A context is simply interpreted as an additional (hidden) argument that contains all the methods. Again, there is no instantiation of polymorphic functions, so classes where the methods are polymorphic do not work as expected.

It is also possible to query for an instance of a class, which is executed as `q` query for each of the methods, e.g.

```hs
Djinn> ?instance Monad Maybe
instance Monad Maybe where
  return = Just
  (>>=) a b =
    case a of
      Nothing -> Nothing
      Just c -> b c
```
