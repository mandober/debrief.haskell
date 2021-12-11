# Data.Functor

```hs
-- Data.Functor.$>
(<$>) :: Functor f => (a -> b) -> f a -> f b -- fmap as operator
($>)  :: Functor f => f a      -> b   -> f b -- flipped (<$)
(<$)  :: Functor f => a        -> f b -> f a
void  :: Functor f => f a             -> f ()
-- Data.Functor.<&>
(<&>) :: Functor f => f a -> (a -> b) -> f b -- flipped (<$>)

type Functor :: (Type -> Type) -> Constraint
class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  {-# MINIMAL fmap #-}
```

## void

* `void` value discards or ignores the result of evaluation,
  such as the return value of an `System.IO.IO` action.

```hs
void :: Functor f => f a -> f ()
```

* `void` is defined in `Data.Functor`, reexported by `Control.Monad`.

```hs
import Data.Functor (void)
-- or
import Control.Monad (void)
```


* Using *ApplicativeDo*: `void as` can be understood as the do expression:

```hs
… do as
     pure ()
```

with an inferred `Functor` constraint.

# Unused bindings in a do-block

With the flag like `-Wall` enabled, or specifically with `-Wunused-do-bind`, GHC issues warnings about the unused bindings in the *do-blocks*.

For example, writing a Parsec parser that should parse (but discard) parens and the ending dot symbol:

```hs
parseNumber :: Parser Parens
parseNumber = do
  char '('            -- (1)
  d <- many1 digit
  _ <- char ')'       -- (2)
  void $ char '.'     -- (3)
  return $ Parens $ read d

-- warning: [-Wunused-do-bind]
-- A do-notation statement discarded a result of type Char
-- Suppress this warning by saying
--    _ <- char '('
--    …
```

with like warning turned on, the line (1) will trigger the -Wunused-do-bind warning. The lines (2) and (3) show two ways how to supress it. The (2) is handy as, unlike (3) it requires no imports, but it's not always convenient to stick in; e.g.

```hs
whitespaces :: Parser ()
whitespaces = void $ many $ oneOf " \n\t"
```


# Examples

```hs
-- Replace the contents of 'Maybe Int' with ()
>>> void Nothing
Nothing
>>> void (Just 3)
Just ()

-- Replace contents of 'Either Int Int' with () resulting in 'Either Int ()'
>>> void (Left 8675309)
Left 8675309
>>> void (Right 8675309)
Right ()

-- Replace every element of a list with ()
>>> void [1,2,3]
[(),(),()]

-- Replace the second element of a pair with ()
>>> void (1,2)
(1,())

-- Discard the result of an System.IO.IO action
>>> mapM print [1,2]
1
2
[(),()]
>>> void $ mapM print [1,2]
1
2
```
