# Guide to morphisms

## Least fixpoint (Data)

- Least fixed point is related to *data*
- data is finite, codata is possibly infinite
- Least fixed point defines a *initial algebra*

```hs
(Type -> Type) -> Type

μf = f (μf) = f (… (μf) … ) = Free f 0
```

Instances of `μf` are "f-data-structures" or "f-structures" for short.

```hs
fix f = f (fix f)
fix f = f (f (fix f))         -- since fix f = f (fix f)
fix f = f (… (f (fix f)) … )
```

## Free Monads

```hs
(Type -> Type) -> Type -> Type

Free f a = a + f (Free f a)
          └┬┘  └─────┬────┘
       variable     term
```

- A finite f-structure, that can contain `a`s
- It is a functor and a monad
- Monadic-bind corresponds to substitution
- it substitutes `a`s by terms that can contain `b`s

## Greatest fixed point (Codata)

- greatest fixed point is related to *codata*
- data is finite, codata is possibly infinite
- greatest fixed point defines a *final coalgebra*

```hs
(Type -> Type) -> Type

νf = f (νf) = f (f (…)) = Cofree f 1
```

Instances of `νf` are "f-codata-structures" or "f-structures" for short.

(`ν` is the Greek letter "ni")

## Cofree Comonads

```hs
(Type -> Type) -> Type -> Type

Cofree f a = a , f (Cofree f a)
            └┬┘  └─────┬────┘
      annotation     trace
```

- A possibly infinite f-structure, full of `a`s
- It is a functor and a comonad
- Comonadic `extend` corresponds to computing a new f-structure full of `b`s
- At every level `a` and the full trace are available for computing the `b`s

## Recursion Schemes - Destruction Morphisms

### Catamorphism

```hs
cata :: ∀ a. (f a -> a) -> μf -> a
             └───┬────┘
             f-algebra
```

- aka fold
- Deconstructs a f-structure level-by-level and applies the algebra
- see [^13] [^5] [^14] [^6]

### Paramorphism

```hs
para :: ∀ a. (f (μf , a) -> a) -> μf -> a
```

- aka "the Tupling-Trick"
- Like cata, but allows access to the full subtree during teardown
- it is a special case of zygo, with the helper being the initial-algebra
- see [^16]

### Zygomorphism

```hs
zygo :: ∀ a b. (f (a , b) -> a)
     -> (f b -> b)
     -> μf
     -> a
```

- allows depending on a helper algebra for deconstructing a f-structure
- A generalisation of para


### Histomorphism

```hs
histo :: ∀ a. (f (Cofree f a) -> a) -> μf -> a
```

- Deconstructs the f-structure with the help of all previous computation for the substructures (the trace)
- Difference to para: the subcomputation is already available and needs not to be recomputed.

### Prepromorphism

```hs
prepro :: ∀ a. (f a -> a) -> (f ⇝ f) -> μf -> a
```

- Applies the natural transformation at every level, before destructing with the algebra
- Can be seen as a one-level rewrite
- This extension can be combined with other destruction morphisms
- see [^4]

## Recursion Schemes - Construction Morphisms

### Anamorphism

```hs
ana :: ∀ a. (a -> f a) -> a -> νf
             └───┬───┘
            f-coalgebra
```

- aka unfold
- Constructs a f-structure level-by-level, starting with a seed and repeatedly applying the coalgebra
- see [^13] [^5]


### Apomorphism

```hs
apo :: ∀ a. (a -> f (a + νf)) -> a -> νf
```

- aka "the Co-Tupling-Trick"™
- Like ana, but also allows to return an entire substructure instead of one level only.
- it is a special case of g-apo, with the helper being the final-coalgebra
- see [^17] [^16]

### G-Apomorphism

```hs
gapo :: ∀ a b. (a -> f (a + b))
     -> (b -> f b)
     -> a
     -> νf
```

- Allows depending on a helper coalgebra for constructing a f-structure.
- A generalisation of apo.


### Futumorphism

```hs
futu :: ∀ a. (a -> f (Free f a)) -> a -> νf
```

- Constructs a f-structure stepwise, but the coalgebra can return multiple layers of a-valued substructures at once.
- Difference to apo: the subtrees can again contain as [^16]

### Postpromorphism

```hs
postpro :: ∀ a. (a -> f a) -> (f ⇝ f) -> a -> νf
```

- Applies the natural transformation at every level, after construction with the coalgebra.
- Can be seen as a one-level rewrite.
- This extension can be combined with other construction morphisms


## Recursion Schemes - Combined Morphisms

### Hylo

```hs
hylo :: ∀ a b. (a -> f a) -> (f b -> b) -> a -> b
```

- hylomorphism = ana then cata
- Omits creating the intermediate structure and immediately applies the algebra to the results of the coalgebra
- Can also be enhanced by a representation change (natural transformation `f ⇝ g`), before deconstructing with a corresponding g-algebra
- see [^13] [^2] [^5] [^14]

### Dyna

```hs
dyna :: ∀ a b. (a -> f a)
     -> (f (Cofree f b) -> b)
     -> a
     -> b
```

- dynamorphism = ana then histo
- Constructs a structure and immediately destructs it while keeping intermediate results.
- Can also be enhanced by a representation change (natural transformation `f ⇝ g`), before deconstructing with a corresponding g-algebra
- Can be used to implement dynamic-programming algorithms
- see [^9] [^10]


### Crono

```hs
chrono :: ∀ a b. (a -> (Free f a))
       -> (f (Cofree f b) -> b)
       -> a
       -> b
```

- chronomorphism = futu then histo
- Can at the same time "look back" at previous results and "jump into the future" by returning seeds that are multiple levels deep
- Can also be enhanced by a representation change (natural transformation `f ⇝ g`), before deconstructing with a corresponding g-algebra
- see [^11].


### Meta

```hs
meta :: ∀ a b. (f a -> a) -> (a -> b) -> (b -> g b)
     -> μf
     -> νg
```

- metamorphism = cata then conversion then ana
- Constructs a g-structure from a f-structure while changing the internal representation in-between
- see [^7]


## Recursion Schemes - Other Morphisms

- GAlgebra (generalized algebra)
- GCoalgebra (generalized coalgebra)
- Distr (distributive algebra)

Most of the above morphisms can be modified to accept generalized algebras, with `w` being a Comonad:

```hs
GAlgebra f w a = f (w a) -> a
```

or generalised coalgebras, with `m` being a Monad:

```hs
GCoalgebra f m a = a -> f (m a)
```

A multitude of other morphisms exist, see [^12] [^3] [^1], and the combination of morphisms and distributive laws has been studied in [^8] [^15].

```hs
Distr f g = ∀ a. f (g a) -> g (f a)
```

## References

[^1]: Adámek, Jiří, Stefan Milius, and Jiří Velebil. __Elgot algebras__. Electronic Notes in Theoretical Computer Science, 2006.

[^2]: Augusteijn, Lex. __Sorting morphisms__. Advanced Functional Programming. Springer Berlin Heidelberg, 1998.

[^3]: Erwig, Martin. __Random access to abstract data types__, Springer Berlin Heidelberg, 2000.

[^4]: Fokkinga, Maarten M. __Law and order in algorithmics__, PhD Thesis, 1992.

[^5]: Gibbons, Jeremy. __Origami programming__, 2003.

[^6]: Gibbons, Jeremy. __Design patterns as higher-order datatype-generic programs__ . Proceedings of the Workshop on Generic programming. ACM, 2006.

[^7]: Gibbons, Jeremy. __Metamorphisms: Streaming representation-changers__. Science of Computer Programming, 2007.

[^8]: Hinze, Ralf, et al. __Sorting with bialgebras and distributive laws__. Proceedings of the Workshop on Generic programming. ACM, 2012.

[^9]: Hinze, Ralf, and Nicolas Wu. __Histo-and dynamorphisms revisited__. Proceedings of the Workshop on Generic programming. ACM, 2013.

[^10]: Kabanov, Jevgeni, and Varmo Vene. __Recursion schemes for dynamic programming__. Mathematics of Program Construction. Springer Berlin Heidelberg, 2006.

[^11]: Kmett Edward. __Time for Chronomorphisms__, 2008. http://comonad.com/reader/2008/time-for-chronomorphisms/

[^12]: Kmett Edward. __Recursion Schemes: A Field Guide (Redux)__, 2009. http://comonad.com/reader/2009/recursion-schemes/

[^13]: Meijer, Erik, Maarten Fokkinga, and Ross Paterson. __Functional programming with bananas, lenses, envelopes and barbed wire__. Functional Programming Languages and Computer Architecture. Springer Berlin Heidelberg, 1991.

[^14]: Oliveira, Bruno, and Jeremy Gibbons. __Scala for generic programmers__. Proceedings of the Workshop on Generic programming. ACM, 2008.

[^15]: Turi, Daniele, and Gordon Plotkin. __Towards a mathematical operational semantics__. Logic in Computer Science. IEEE, 1997.

[^16]: Uustalu, Tarmo, and Varmo Vene. __Primitive (co) recursion and course-of-value (co)iteration, categorically__. Informatica, 1999.

[^17]: Vene, Varmo, and Tarmo Uustalu. __Functional programming with apomorphisms (corecursion)__. Proceedings of the Estonian Academy of Sciences: Physics, Mathematics. Vol. 47. No. 3. 1998.
