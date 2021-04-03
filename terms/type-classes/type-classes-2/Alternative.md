# Alternative class

https://en.wikibooks.org/wiki/Haskell/Alternative_and_MonadPlus

Both Maybe and lists can represent computations with a varying number of results. We use Maybe to indicate a computation can fail somehow (that is, it can have either zero results or one result), and we use lists for computations that can have many possible results (ranging from zero to arbitrarily many results).

In both of these cases, one useful operation is merging of all possible results from multiple computations into a single computation. With lists, for instance, that would amount to concatenating lists of possible results. The Alternative class captures this merger in a general way.

Alternative class and its methods are in the `Control.Applicative` module.

```hs
import Control.Applicative

class (Applicative f) => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a
    -- MINIMAL: empty, (<|>)

    -- many and some are defined in terms of each other:
    many :: f a -> f [a]
    many x = some x <|> pure []
    
    some :: f a -> f [a]
    some x = pure (:) <*> x <*> many x
```

The `empty` and `<|>` are the two methods that must be defined for a minimal definition. `some` and `many` you get for free.

* `empty` is an applicative computation with zero results
* <|> (the choice) is a binary function which combines two computations
* `some` is a computation that captures 1 or more results, like regex "+"
* `many` is a computation that captures 0 or more results, like regex "*"



## asum

A common task when working with Alternative is taking a list of alternative values, e.g. [Maybe a] or [[a]], and folding it down with (<|>). The function asum, from Data.Foldable fulfills this role:

```hs
asum :: (Alternative f, Foldable t) => t (f a) -> f a
asum = foldr (<|>) empty
```

In a sense, asum generalizes the list-specific concat operation. Indeed, the two are equivalent when lists are the Alternative being used. For Maybe, asum finds the first Just x in the list and returns Nothing if there aren't any.

msum, available from both `Data.Foldable` and `Control.Monad`, is just asum specialised to MonadPlus.

```hs
msum :: (MonadPlus m, Foldable t) => t (m a) -> m a
```

## guard
https://en.wikibooks.org/wiki/Haskell/Alternative_and_MonadPlus
When discussing the list monad we noted how similar it was to list comprehensions, but we didn't discuss how to mirror list comprehension filtering. The guard function from `Control.Monad` allows us to do exactly that.

Consider the following comprehension which retrieves all pythagorean triples:

```hs
pythags = [(x, y, z) | z <- [1..], x <- [1..z], y <- [x..z], x^2 + y^2 == z^2]


-- The translation of the comprehension to a list monad do-block is:
pythags = do
    z <- [1..]
    x <- [1..z]
    y <- [x..z]
    guard (x^2 + y^2 == z^2)
    return (x, y, z)


-- The guard function can be defined for all Alternatives like this:
guard ::( Alternative m) => Bool -> m ()
guard True = pure ()
guard _    = empty
```

guard will reduce a do-block to `empty` if its predicate is False. Given the left zero law:     
`mzero >>= f = mzero` or, equivalently: `empty >>= f = empty`    
an `empty` on the LHS of (>>=) will produce `empty` again. As do-blocks are decomposed to lots of expressions joined up by (>>=), an `empty` at any point will cause the entire do-block to become `empty`.
