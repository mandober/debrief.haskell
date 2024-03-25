

The `fmap` function is the primary method, the reason d'etre of the `Functor` class. Howevere, in the more category-theoretical precise terms, a functor is a mapping between two categories that maps (1) objects to objects and (2) arrows to arrows. In Haskell, this translates to (1) the mapping of types to types (types are objects): a type ctor `f` acts as a type mapper, mapping one type to another; e.g. `Maybe` type ctor maps a type like `Int` to the type `Maybe Int`; and (2) functions are mapped to functions, so a function `f :: a -> b` is mapped to a function `fmap f :: f a -> f b`, also called *lifting* of the function `f`.

e.g. the function `length :: [Int] -> Int` is mapped to 
`fmap length :: Maybe [Int] -> Maybe Int`
