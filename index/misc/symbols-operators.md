# Operator names

- https://wiki.haskell.org/Pronunciation
- https://stackoverflow.com/questions/7746894/are-there-pronounceable-names-for-common-haskell-operators
- https://stackoverflow.com/questions/3242361/haskell-how-is-pronounced
- https://www.euclideanspace.com/software/language/functional/haskell/operators/index.htm
- https://wiki.haskell.org/wikiupload/b/b2/QuickReference.pdf
- https://hackage.haskell.org/package/CheatSheet-1.11/src/CheatSheet.pdf
- https://wiki.haskell.org/How_to_work_on_lists
- https://tech.fpcomplete.com/haskell/tutorial/operators



Possible names:

```hs
>>= :: bind
>>  :: then
*>  :: then
->  :: to                a -> b: a to b
<-  :: bind              (as it desugars to >>=)
<$> :: (f)map
<$  :: map-replace by    0 <$ f: "f map-replace by 0"
<*> :: ap(ply)           (as it is the same as Control.Monad.ap)
$   :: apply
.   :: after             g . f: "g after f"
!!  :: index
!   :: index / strict    a ! b: "a index b", foo !x: foo strict x
<|> :: or / alternative  expr <|> term: "expr or term"
++  :: concat / plus / append
[]  :: empty list
:   :: cons
::  :: of type / as      f x :: Int: f x of type Int
\   :: lambda
@   :: as                go ll@(l:ls): go ll as l cons ls
~   :: lazy              go ~(a,b): go lazy pair a, b
```



| sym  | pronunciation
|------|--------------
| |    | such that
| <-   | slurp, is drawn from
| =    | is defined to be, is defined as
| ::   | has type, of type, is of type
| ->   | function ctor, to, if...then
| $    | apply                                          |
| _    | hole, forgitaboutit, whatever
| !!   | index                                          |
| ++   | concat                                         |
| []   | empty list                                     |
| :    | cons                                           |
| \    | lambda                                         |
| =>   | implies / then                               |
| *>   | then                                           |
| <$>  | fmap / dollar cyclops                        |
| <$   | map-replace by                                 |
| <*>  | ap / star cyclops                            |
| .    | pipe to / compose / dot                    |
| <|>  | or                                             |
| @    | as                                             |
| ~    | lazy                                           |
| <=<  | left fish                                      |



## Haskell Operators and other Lexical Notation

```
--    Start of comment line
{-    Start of short comment
-}    End of short comment

+     Add operator
-     Subtract/negate operator
*     Multiply operator
/     Division operator

/     Substitution operator, as in e{f/x}

(^)   power: Num ^ Integral
(^^)  power: Fractional ^^ Integral
(**)  power: Floating ** Floating

&&    And operator
||    Or operator

<     Less-than operator
<=    Less-than-or-equal operator
>     Greater-than operator
>=    Greater-than-or-equal operator
==    Equal operator
/=    Not-equal operator

\     Lambda abstraction head
.     Function composition operator
.     Name qualifier
|     Guard and case specifier
|     Separator in list comprehension
|     Alternative in data definition (enum type)

++    List concatenation operator
:     Append-head operator, cons
!!    Indexing operator
..    Range-specifier for lists
\\    List-difference operator
<-    List comprehension generator
<-    Single assignment operator in `do` constr
;     Definition separator

->    Function type-mapping operator
->    Separator in case construction
->    Lambda abstraction body

=     Type- or value-naming operator
::    Type specification operator, "has type"
=>    Context inheritance from class

>>    Monad sequencing operator
>>=   Monad sequencing operator with value passing
>@>   Object composition operator (monads)
(..)  Constructor for export operator (postfix)

[]    List constructors, (,) as separator
,     Tuple constructors, (,) as separator
()    Infix-to-prefix constructors, e.g. (+)
()    Empty value in IO () type
()    unit type and value (empty tuple)

`     `Prefix` to infix constructors, e.g. `div`
'     Literal char constructors, e.g. 'c'
"     String constructors, e.g. "string"

@     "Bind all as", "Read As" in pattern matching
_     Wildcard in pattern
_     Typed hole (partial signatures)
~     Irrefutable pattern
!     Force evaluation (strictness flag)
```


## Operators consisting of special characters

```hs
(==) ::  Eq a => a -> a -> Bool
(/=) ::  Eq a => a -> a -> Bool

(<)  :: Ord a => a -> a -> Bool
(<=) :: Ord a => a -> a -> Bool
(>)  :: Ord a => a -> a -> Bool
(>=) :: Ord a => a -> a -> Bool

(&&) :: Bool -> Bool -> Bool         -- infixr 3 &&
(||) :: Bool -> Bool -> Bool         -- infixr 2 ||

(!!) :: [a] -> Int -> a

(+)  :: Num a => a -> a -> a          -- infixl 6 +
(-)  :: Num a => a -> a -> a          -- infixl 6 -
(*)  :: Num a => a -> a -> a          -- infixl 7 *
(/)  :: Fractional a => a -> a -> a   -- infixl 7 /
(**) :: Floating a => a -> a -> a     -- infixr 8 **

($)  :: (a -> b) -> a -> b            -- infixr 0 $! -- apply low assoc, lazy
($!) :: (a -> b) -> a -> b            -- infixr 0 $! -- apply low assoc, strict
(.)  :: (b -> c) -> (a -> b) -> a -> c
(++) :: [a] -> [a] -> [a]             -- infixr 5 ++

(<$)  :: Functor f => a -> f b -> f a
(<$>) :: Functor f => (a -> b) -> f a -> f b

(<*)  :: Applicative f => f a -> f b -> f a
(*>)  :: Applicative f => f a -> f b -> f b         -- infixl 4
(<*>) :: Applicative f => f (a -> b) -> f a -> f b

(>>)  :: Monad m => m a -> m b -> m b
(>>=) :: Monad m => m a -> (a -> m b) -> m b
(=<<) :: Monad m => (a -> m b) -> m a -> m b
```
