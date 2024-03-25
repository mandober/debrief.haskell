# Type promotion

https://diogocastro.com/blog/2018/10/17/haskells-kind-system-a-primer/

In standard Haskell, the `data` keyword lets us define a custom typ (type constructor) followed by a set of data constructors. But when GHC's `DataKinds` extension is enabled, the `data` keyword creates two additional things: a custom kind, followed by a set of type constructors.

```hs
{-# LANGUAGE DataKinds #-}

data ConnectionStatus = Open | Closed

> :t Open
Open :: ConnectionStatus

> :k 'Open
'Open :: ConnectionStatus
```

In the code above, a new kind named `ConnectionStatus` is created. This kind has exactly two *uninhabited types*, `'Open` and `'Closed`.

We say the type `ConnectionStatus` has been promoted to a kind, and `Open` and `Closed` data ctors to types `'Open` and `'Closed` (notice a tick). The tick can almost always be omitted, and only in rare scenarios will you need it for disambiguation:
https://downloads.haskell.org/~ghc/8.4.3/docs/html/users_guide/glasgow_exts.html#distinguishing-between-types-and-constructors

With the new custom kind, we can define a type variable that can only ever be instantiated to `Open` or `Closed`, and nothing else.

```hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

data Connection (s :: ConnectionStatus) = MkConnection ...

newConnection     :: Address           -> Connection Closed
openConnection    :: Connection Closed -> Connection Open
closeConnection   :: Connection Open   -> Connection Closed
connectionAddress :: Connection s      -> Address

λ> :k Connection Int
<interactive>:1:12: error:
    • Expected kind 'ConnectionStatus', but 'Int' has kind '*'
```

By tagging the connection with its status, we can statically enforce rules such as "closeConnection cannot be called on an already closed connection".

This also works for other types; in the presence of *DataKinds*, `Bool`, for example, gets promoted to a kind as well.

```hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

λ> data F (b :: Bool) = MkF deriving Show

λ> MkF :: F 'True
MkF

λ> MkF :: F 'False
MkF

λ> MkF :: F 'Open
<interactive>:30:10: error:
    • Expected kind 'Bool', but ' 'Open' has kind 'ConnectionStatus'
```


## GHC.TypeLits

GHC provides two other very convenient kinds out of the box, in `GHC.TypeLits` module in base.

The first is `Symbol`, which is the kind for type-level strings. It lets us use string literals like `"hello"` as a type. With it, we can tag a type with a string literal.

```hs
{-# LANGUAGE KindSignatures, DataKinds #-}

import GHC.TypeLits (Symbol)
import Data.Ratio ((%))

newtype Money (currency :: Symbol) = Money Rational

fivePence :: Money "GBP"
fivePence = Money (5 % 100)

twoEuros :: Money "EUR"
twoEuros = Money 2
```

Having the currency represented at the type-level, rather than at the term-level (like in data Money = Money String Rational), lets us statically ensure that monies of different currencies don't get mixed up, e.g. we can't add EUR and GBP together.

```hs
add :: Money c -> Money c -> Money c
add (Money x) (Money y) = Money (x + y)
λ> add fivePence fivePence
Money (1 % 10)

λ> add fivePence twoEuros
<interactive>:8:15: error:
    • Couldn't match type '"EUR"' with '"GBP"'
```

If we had the currency at the term-level instead, we'd have to do runtime checks and every function would have to signal the possibility of failure by returning Maybe, Either, MonadError e m, etc.

This is exactly how the safe-money library represents dense  monetary values:
https://hackage.haskell.org/package/safe-money-0.7/docs/Money.html#t:Dense

When we don't need to perform arithmetic operations on money, storing the value as a Rational is a bit overkill. For these scenarios, the library provides a different type, Discrete, which is a simple wrapper around an Integer.


`Nat` is the kind for type-level natural numbers, also exported from *GHC.TypeLits*. It is similar to `Symbol` and it lets us use numeric literals as types.

```hs
{-# LANGUAGE KindSignatures, DataKinds #-}

import GHC.TypeLits (Symbol, Nat)

newtype Discrete (currency :: Symbol) (scale :: (Nat, Nat))
  = Discrete Integer

oneDollar :: Discrete "USD" '(1, 1)
oneDollar = Discrete 1

oneDollarThirtyCents :: Discrete "USD" '(100, 1)
oneDollarThirtyCents = Discrete 130
```

Yes, you saw that right. In `scale :: (Nat, Nat)`, `(,)` is the tuple type promoted to a kind via DataKinds. And in `'(100, 1)`, `'(,)` is the tuple data constructor promoted to a type constructor.
