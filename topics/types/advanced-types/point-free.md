# Point-free style

- use composition
- try sectioning, moving ops to prefix position
- if the RHS ends up empty, try placing `id` there
- often ($) can be replaced with (.), and some tinkering
- use combinatory logic as a guide: Haskell is λ-calculus, any λC expr can be translated into CL, CL elides fn abstraction and therefore points as well


```hs
(++) [] ys = ys
-- ) []    = ??
(++) []    = id
```

Replacing ($) with composition (.)

```hs
f xs = length $ show xs
f xs = (length . show) xs
f    = (length . show)
f    =  length . show
```


## Conversion from λ-calculus to SKI which uses only

https://wiki.haskell.org/Pointfree

SKI calculus is a combinatory calculus that uses only:
* `S` or `<*>` is `λfgx.fx(gx)` , Haskell: `\f g x -> f x (g x)`
* `K`          is `λab.a`       , Haskell: `\a b -> a` or `const`
* `I`          is `λx.x`        , Haskell: `\ x -> x` or `id`

Conversion from λ-calculus to SKI can be done with the following rules:
* \x -> x              = id
* \x -> y ∧ x ∉ FV(y)  = const y
* \x -> f g            = f' <*> g'
  where f' = \x -> f
        g' = \x -> g


There is a special case of the last translation:
* if `f` does not have any free occurrences of `x`    
  then `(\x -> f g)` translates to:   
  `const f <*> (\x -> g)` === `f . (\x -> g)`


Using those rules to convert the problem function:

```hs
f = \x -> ((+) 5) (((/) 8) x)      -- by the special-case (.) rule
  = ((+) 5) . (\x -> (((/) 8) x))  -- by eta-reduction ((\x -> f x) = f)
  = ((+) 5) . ((/) 8)
```

Eta-reduction is not necessary to complete the translation,
but without it there'd be more mess, i.e. the last step would be:
`(5+) . (8/) . id`


https://stackoverflow.com/questions/8465575/simple-haskell-functions-in-point-free-style






---

In the wider sense all Haskell's functions are equations, but in a more narrow sense, since a function may be given in pieces, an equation is a piece comprising the multi-part function.

A function takes some input values and returns an output value.

*Parameter declaration* is the mechanism which allows a function to import values, required to perform its computation, from the outside environment.

A function imports the outside values through its *binding site*, which fills  is located on the left-hand side (LHS), between a function's name and the equals sign.


The import mechanism of a function is realized as the binding site that captures the values 

parameter 


declare a list of parameters required to perform its job by placing a list of identifiers in its *binding site*, which is located on the left-hand side (LHS) to the equals sign.




   head            body
 /      \   =    /     \
f  k  a  b   =   a  `k`  b
↑  ↑  ↑  ↑       ↑   ↑   ↑
|  |  |  |       |   |   actual param 'b'
|  |  |  |       |   actual param 'k'
|  |  |  |       actual param 'a'
|  |  |  |
|  |  |  formal param 'b'
|  |  |
|  |  formal param 'a'
|  |
|  formal param 'k'
|
name of the function, 'f'



equal sign splits a function into a head and a body



       ↑     ↑
       |     body of the function
       binding site



, that may contain a list of *formal parameters*.

accept input values is called 

The mechanism which allows a function to accept input values is called *parameter declaration* and it is realized via a function's the binding site,


A function declares parameters, required to perform its job, in the binding site,



Each piece of a multi-part funcation, must declare the same (number of) params.

It may declare any number of params, incuding none, called *arity* or *adicity* or *adinity*.

Since the list of declared parameters is similar to a list 

The , which is always a fixed number (variadic functions are disallowed).

in the binding site, which is located on the left-hand side of the equals sign (LHS).

it its formal parameters

 while the RHS may contain (if it doesn't mention)
corresponding


Crudely, to convert an equation into its point-free form:   
if the right-most formal param on the RHS
whose corresponding actual formal param on the LHS
is isolated in the right-most location (it is the last thing on the line),
then they both may be elided from the equation.
This may be done for additional, even all, formal params.

Putting a formal param as the last one is easily done for own functions, but doing this just to get a point-free form is not done because the primary factor in the order of params should be mindfullness of partial application and composition, following the rule: operations first, data last.

In many cases, an equation's RHS can be reworked to move the desired actual param to the rightmost position of an equation, using sectioning, lambda forms, infix position, composition.



If the RHS ends up empty, a possible solution is `id`!

```hs
(++) [] ys = ys
-- removing the point from both sides
-- (++) [] = ??
(++) [] = id
```

Replacing ($) with composition (.)

```hs
f xs = length $ show xs
f xs = (length . show) xs
f    = (length . show)
f    =  length . show
```
