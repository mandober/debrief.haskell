# Dump generated code for instance declarations

Passing the flag `-ddump-deriv` will make GHC dump the generated code for instance declarations.

It is immediately clear that GHC takes advantage of safe coercions, consistently using `coerce` to convert between a newtype (`Mile`) and the wrapped type (`Int`). The `coerce` function does require visible type application (VTA), but far far less then in this OCD tantrum below.

This is probably the tactic employed for many other similar types, particularly newtypes, that are simple as this one.


## The common preamble

```hs
-- the main flag
{-# OPTIONS_GHC -ddump-deriv #-}

-- optional noise surpressors
{-# OPTIONS_GHC
    -dsuppress-idinfo
    -dsuppress-coercions
    -dsuppress-type-applications
    -dsuppress-uniques
    -dsuppress-module-prefixes
#-}
```

## Ex.1 Deriving Eq for a newtype declaration

```hs
newtype Mile = MkMile Int deriving (Eq, Ord)

-- ==================== Derived instances ====================
-- Derived class instances:
instance Eq Mile where
  (==) :: Mile -> Mile -> Bool
  (==) = coerce @(Int -> Int -> Bool) @(Mile -> Mile -> Bool) ((==) @Int)

  (/=) :: Mile -> Mile -> Bool
  (/=) = coerce @(Int -> Int -> Bool) @(Mile -> Mile -> Bool) ((/=) @Int)

instance Ord Mile where
  (<) :: Mile -> Mile -> Bool
  (<) = coerce @(Int -> Int -> Bool) @(Mile -> Mile -> Bool) ((<) @Int)

  (<=) :: Mile -> Mile -> Bool
  (<=) = coerce @(Int -> Int -> Bool) @(Mile -> Mile -> Bool) ((<=) @Int)

  (>)  :: Mile -> Mile -> Bool
  (>) = coerce @(Int -> Int -> Bool) @(Mile -> Mile -> Bool) ((>) @Int)

  (>=) :: Mile -> Mile -> Bool
  (>=) = coerce @(Int -> Int -> Bool) @(Mile -> Mile -> Bool) ((>=) @Int)

  max :: Mile -> Mile -> Mile
  max = coerce @(Int -> Int -> Int) @(Mile -> Mile -> Mile) (max @Int)

  min :: Mile -> Mile -> Mile
  min = coerce @(Int -> Int -> Int) @(Mile -> Mile -> Mile) (min @Int)

  compare :: Mile -> Mile -> Ordering
  compare = coerce @(Int -> Int -> Ordering)
                   @(Mile -> Mile -> Ordering)
                   (compare @Int)
```

## Ex.2 Deriving Eq for a data declaration

The same type as the previous one, only replacing `newtype` with `data`. This change breaks the isomorphism between a type and its newtype wrapper, so coercion canot be used. Brace(s) for a flashback!

```hs
data Mile = MkMile Int deriving (Eq, Ord)

-- ==================== Derived instances ====================
-- Derived class instances:
instance Eq Mile where
  (==) (MkMile a1) (MkMile b1) = ((a1 == b1))                         -- (1)

instance Ord Mile where
  compare a b = case a of {                                           -- (2)
    MkMile a1 -> case b of {
      MkMile b1 -> (a1 `compare` b1)
    }
  }

  (<) a b = case a of {                                               -- (3)
    MkMile a1 -> case b of {
      MkMile b1 -> (a1 < b1)
    }
  }

  (>)  a b = (<) b a                                                  -- (4)
  (<=) a b = not ((<) b a)                                            -- (5)
  (>=) a b = not ((<) a b)

  -- ==================== Filling in method body ====================
 Eq  [Mile]   /= = $dm/=  @(Mile)                                     -- (6)
 Ord [Mile]  max = $dmmax @(Mile)
 Ord [Mile]  min = $dmmin @(Mile)
-- elided methods! TH!
```

Observations:
1. The instance for `Eq` is quite a commonplace: it delegates the `==` method to the underlying type (`Int`), after unwrapping the data ctor (`MkMiles`) using the usual left-hand-sided pattern match.
2. The `Ord` instance is more interesting, but also common: for some reason, `compare` switches to a case analysis of each arg, abandoning pattern matching.
3. Cae analysis is also used in the definition of the `<` method.
4. Change of tactics: the `>` relies on the previously defined `<` method, from the same class, `Ord`.
5. The delegation tactic continues in the case of `<=` and `>=`, only now both methods rely on the `/=` method from a different class, `Eq`, that was also previously auto-derived.
6. This has something to do with class' methods that lack an explicit definition. Namely, many classes specify the minimal set of methods you need to implement for a type so it becomes a member of that class. But then the compiler must define those elided methods, so it generates these strange codes. That is also the case here: the elided method `/=` of `Eq` and `max` and `min` of `Ord` are defined using this, I guess, template Haskell approach. Also, god knows and I'm not gonna check what the generated code really looks like in full; something is probably not displayed here regarding the TH bits of code.
