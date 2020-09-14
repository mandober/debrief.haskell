# Function

In Unix, everything is a file, in some OO languages, everything is an object and in FP languages everything is a function.

The thing is, everything could be implemented as a function. For example, numeric literals could be constant functions, `n2 _ = 2`, that is, thunks that ignore the param, always returning the fixed number. Lists *are* practically functions themselves:

A function can also be defined by explicitly stating 

an arbitrary set of ordered pairs, `[(x₀,y₀), (x₁,y₁), (x₂,y₂), ...]` that correspond to the `x` and `y` Cartesian coordinates; the first component of a pair is the input to a function, i.e. the argument bound by the parameter `x`; the second component, `y`, is the function's output, i.e. function's value for the given input.



A function is a
- correspondance
- association
- process
- graph i.e. the listing of all ordered pairs
- procedure
- ruleset
- transformation
- transformation rule
- law


A function is a binary relation over two sets that associates to every element of the first set exactly one element of the second set.

A function is a process or a relation that associates each element x of a set X, the domain of the function, to a single element y of another set Y (possibly the same set), the codomain of the function.

In math, if the function is called `f`, it may be denoted by `f (x)=y`, where the parameter `x` represents function's input, while `y` stands for function's output. The `y` variable stands for the value of the function, the value that a function "returns" (to the caller; after being called with an appropriate argument).


A function is uniquely represented by the set of all pairs `(x,y)`, called **the graph of the function**.

When the domain and the codomain are sets of real numbers, each such pair may be considered as the *Cartesian coordinates* of a point in the plane. The set of these points is called **the graph of the function**.

Extensional vs intensional equality


## Some definitions of the concept of a function

I call a function of a variable magnitude a quantity composed in any manner whatsoever from this variable magnitude and from constants.    
-- *Johann Bernoulli, 1718*

A function of a variable quantity is an analytic expression composed in any way whatsoever of the variable quantity and numbers or constant quantities.    
-- *Euler, 1748*

When two quantities depend on each other in such a way that a change in the first results in a change in the second - this is called the function of the first. This is a very comprehensive idea which includes in itself all the ways in which one quantity can be determined by others.    
-- *Euler, 1775*

Every quantity whose value depends on one or more quantities is called a function of these latter, whether one knows or is ignorant of what operations it is necessary to use to arrive from the latter to the first.    
-- *Lacroix, 1810*

In general the function f (x) represents a succession of values or ordinates each of which is arbitrary. An infinity of values being given to the abscissa x, there is an equal number of ordinates f (x). All have actual numerical values, either positive or negative or null. We do not suppose these ordinates to be subject to a common law; they succeed each other in any manner whatever, and each of them is given as if it were a single quantity.    
-- *Fourier, 1822*

A single-valued function of a variable x is an expression which for every single rational or irrational value of x is uniquely determined.    
-- *Heine, 1872*

A function `φ` on a set `S` is a law according to which to every determinate element `s` of `S` there belongs a determinate thing which is called the transform of `s` and denoted `φ(s)`.    
-- *Dedekind, 1888*
