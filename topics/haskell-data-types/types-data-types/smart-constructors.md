# Smart constructors

https://wiki.haskell.org/Smart_constructors#Enforcing_the_constraint_statically

Smart constructors are a Haskell's programming idiom that names the concept of placing more constraints on the construction of values than what the data constructors and type system allow.

## Example

Consider the following problem: we want to be able to specify a data type for electronic resistors that come in two forms, metal and ceramic, and are labelled with a number of bands, from 4 to 8. We'd like to ensure only resistors with the right number of bands are ever constructed.

In a naive approach, we design an appropriate datatype that can easily encode the fact that resistors are either ceramic or metallic (benefits of sum types), but it cannot encode the fact that we allow only 4-8 bands because the bands are represented as Int. (BTW, Liquid Haskell can encode this easily).

```hs
type Bands = Int

data Resistor
  = Metal   Bands
  | Ceramic Bands
  deriving Show
```

Now, the not-so-smart-constructors are just functions that conduct extra checks when the value is constructed. The tragedy is that these checks happen at runtime, which is far from the static checking we all strive for. The *encoding of datatype invariants in the type system* is our holy quest.

When exporting the `Resistor` datatype, its unsafe data ctors are not mentioned in the parens after the type, thus they are not exported.

```hs
module Resistor
  ( Resistor          -- abstract type: no data ctor exported
  , metalResistor     -- the smart ctor to build a metal resistor
  -- xor:
  , Resistor (..)     -- all data ctors exported
  , Resistor (Metal)  -- only Metal data ctor exported
  )
  where

-- unholy value construction threatens with panic (on the streets of Carlisle)
metalResistor :: Bands -> Resistor
metalResistor n
  | n < 4 || n > 8 = error "Invalid number of resistor bands" 
  | otherwise      = Metal n
```

## Assertions

Hand-coding error messages can be tedious when used often. Instead, we can use the `Control.Exception.assert` function. At least it gives us automatically generated detailed error messages.

```hs
import Control.Exception (assert)

metalResistor :: Bands -> Resistor
metalResistor n = assert (n >= 4 && n <= 8) $ Metal n
```

## Compile-time checking

The holy goal of our quest for type and value safety is having static checks, i.e. compile-time checks, so even your IDE can warn you when you try to build an illegal and ill-typed value.

Numeric and other type-checking can be done with the type-level arithmetic that can enforce the extra type constraints at compile time.

Instead of checking the band count at runtime, we impl the check at the type level using *phantom types* and *Peano numbers*.

Additional consequences of this approach is faster code due to less RT checks, and, since the band count is represented in the type, it is no longer necessary to carry it around at runtime, meaning less data needs to be allocated.

First, we define Peano numbers to use as the repr of the number of bands as types.

Next, we define a method-less class of cardinal numbers as a way to relate Z and S types and the data ctors of the same name, that is, to connect these two, completely separate types, at least to group them as members of the Cardinal class so they are considered as one, as "Peano-kinded" number types (!tf).

Next, we encode a type-level version of the bounds check (only resistors with 4 <= bands <= 8 are valid) by making a new class called `InBounds`, intended to repr these numeric bounds. We must also repr 4-8 Peano interval as instances of this class.

(Hopefully, all this code can be Template Haskell-ed; if not, you might as well make a big-ass sum type that encodes all this variants)

Next, we redefine the resistor type. Since the bounds are represented in the type, we no longer need to store them in the resistor value.

Finally, we write a convenience constructor function that encodes the bounds check in the type.

```hs
data Z   = Z
data S a = S a

-- label class to connect S and Z
class Card c where

instance Card Z where
instance Card c => Card (S c) where

-- label class to repr bounds
class Card size => InBounds size where

instance InBounds             (S (S (S (S Z)))) where   -- 4
instance InBounds          (S (S (S (S (S Z))))) where   -- 5
instance InBounds       (S (S (S (S (S (S Z)))))) where   -- 6
instance InBounds    (S (S (S (S (S (S (S Z))))))) where   -- 7
instance InBounds (S (S (S (S (S (S (S (S Z)))))))) where   -- 8

-- redefined with a phantom type var
data Resistor size = Resistor deriving Show

-- convenience ctor fn
resistor :: InBounds size => size -> Resistor size
resistor _ = Resistor
```
