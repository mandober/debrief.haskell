# Newtype semantics

https://wiki.haskell.org/Newtype

The `data` can only be replaced with `newtype` if the type has exactly one data ctor with exactly one field inside it.


```hs
newtype Wrapped a = Wrap { unWrap :: a }
```

The restriction to a single data ctor with one field means that the newtype and the type of the field are in direct correspondence, i.e. they are isomorphic.

```hs
-- record syntax is still allowed, but only for one field
newtype State s a = State { runState :: s -> (s, a) }

-- correspondence between the newtype (which wraps a type with a data-ctor)
State :: (s -> (s, a)) -> State s a

-- and its single field (which unwraps the data-ctor)
runState :: State s a -> (s -> (s, a))

data Any = Any { getAny :: Bool }
--         Any :: Bool -> Any
--               getAny:: Any -> Bool
-- getAny . Any :: Bool -> Bool
-- Any . getAny :: Any -> Any
-- Any . getAny . Any == Any
-- getAny . Any . getAny == getAny
```

This means that after the type is checked at compile time, at runtime the two types can be treated essentially the same, without the overhead or indirection normally associated with a data ctor.

The common use case is to declare a different type class instances for a particular type (since a type can only have one instance of the same class, i.e. `Int` can be the instance of `Num` once - as `Int`), or to make a type abstract, you can wrap it in a newtype and it'll be considered distinct to the type-checker, but identical at runtime. You can then use all sorts of deep trickery like *phantom types* or *recursive types* without worrying about GHC shuffling bytes for no reason.

## Semantic difference

However, there is a subtle, yet semantically significant, difference between `data` and `newtype`. When we create a data type, supposedly isomorphic to `Bool` like below, we actually find out that the isomorphism isn't exact.

```hs
data Any = Any { getAny :: Bool }

(Any . getAny . Any) True  = Any True   -- ok
(Any . getAny . Any) False = Any False  -- ok
(Any . getAny . Any) ⊥     = Any ⊥      -- ok
(Any . getAny) ⊥           = Any ⊥      -- wha...'t a second
```

The problem is that types declared with the `data` keyword are _lifted_, that is, they contain their own bottom value that is distinct from all the others.

> The types declared with the `data` keyword are _lifted_, i.e. they contain their own *bottom* value, distinct from all the others.

In this example, we have `⊥ :: Any` distinct from `Any ⊥ :: Any`.

What this means is that this pattern match:

```hs
case x of
  Any _ -> ()
```

**must evaluate its argument**, even though it seems like the pattern match cannot fail!

However, it must check whether `x` is `⊥` or `Any y`, for some `y` (it must check if `x` is bottom or a value tagged with `Any` data ctor).

This is intrinsic to Haskell's lazy, non-total semantics.

The problem is that this means tracking whether a value is wrapped in a data ctor or not, which means keeping track of those extra ctors at runtime even when all they do is *distinguish an extra bottom value we don't even care about*.

Therefore, in order to be resemble some kind of consistency, but also to allow the exact isomorphism to be preserved, Haskell provides the `newtype` keyword, for the construction of unlifted types (to prevent redundant lifting of already lifted types). *Pattern-matching on a newtype constructor doesn't do any work*, because there is no separate ⊥ - every value in the type is wrapped in the data ctor.

## Newtype is ultra-lazy

```hs
data    LAZ = LAZ  Int    -- ctor lazily  refers to an Int
data    STR = STR !Int    -- ctor eagerly refers to an Int
newtype NEW = NEW  Int    -- ctor has the same repr as Int



-- arg is lazy and ignored, so undefined doesn't
-- fail since the ctor pattern match succeeds
x1 = case LAZ undefined of LAZ _ -> 1 -- 1                ✅

-- arg is strict, so undefined fails
x2 = case STR undefined of STR _ -> 1 -- undefined        ⛔

-- newtype behaves like Int
x3 = case NEW undefined of NEW _ -> 1 -- 1                ✅



-- ctor pattern match fails
y1 = case undefined of LAZ _ -> 1 -- undefined            ⛔

-- ctor pattern match fails
y2 = case undefined of STR _ -> 1 -- undefined            ⛔

-- newtype behaves like Int (no ctor at RT)
y3 = case undefined of NEW _ -> 1 -- 1                    ✅



-- Int pattern match succeeds
n1 = case int of _ -> 1 -- 1                              ✅
--         ↓
int ::    Int
int = undefined
```



semantics                   | newtype Foo | data Foo | data Foo
data vs newtype             | = Foo ()    | = Foo () | = Foo !()
----------------------------|-------------|----------|------------
case Foo ⊥ of Foo _ ->   () | ()          | ()       | ⊥
case     ⊥ of Foo _ ->   () | ()          | ⊥        | ⊥
     Foo ⊥         `seq` () | ⊥           | ()       | ⊥
        (⊥ :: Foo) `seq` () | ⊥           | ⊥        | ⊥



## Strict newtypes

A newtype with the strict field annotation, e.g.

```hs
data Identity' a = Identity' !a
```

has `Identity' ⊥ = ⊥` and so you might think you have your coveted isomorphism. But all the strictness annotation means is that `Identity' ⊥` really means `Identity' $! ⊥`, i.e. the semantics of the type are fundamentally the same, and in particular the case-expression still forces the value.
