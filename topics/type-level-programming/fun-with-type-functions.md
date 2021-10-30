# Fun with Type Functions
(paper, 2010)

## 2. Associated types: indexing types by types

Haskell has means to express *relations on types*:
- `multi-parameter type classes` express arbitrary, *many-to-many relations*
- `type constructors` express specifically *functional relations*, where one type (the argument) uniquely determines the other

For example, the relation between the type of a list and the type of that list's elements is a functional relation, expressed by the type ctor `[] :: * -> *`, which maps an arbitrary type `a` to the type `[a]` of lists of `a`.

*Type ctors* map their argument types uniformly, incorporating them into a more complex type without inspection. *Type functions* also establish functional relations between types, but a type function may perform case analysis on its argument types.

### Example

Consider the relation between a monad that supports mutable state and the corresponding type constructor for reference cells.

The `IO` monad supports the following operations on reference cells of type `IORef a`:

```hs
newIORef   :: a -> IO (IORef a)
readIORef  :: IORef a -> IO a
writeIORef :: IORef a -> a -> IO ()
```

Similarly, the `ST s` monad supports the analogous operations on reference cells of type `STRef s a`:

```hs
newSTRef   :: a -> ST s (STRef s a)
readSTRef  :: STRef s a -> ST s a
writeSTRef :: STRef s a -> a -> ST s ()
```

It is tempting to overload these operations using a multiparameter type class:

```hs
class Mutation m r where
  newRef   :: a -> m (r a)
  readRef  :: r a -> m a
  writeRef :: r a -> a -> m ()

instance Mutation IO IORef where
  newRef = newIORef
  -- etc.

instance Mutation (ST s) (STRef s) where
  newRef = newSTRef
  -- etc.
```

This approach has 2 related disadvantages:

1. the types of `newRef` and the other class operations are too polymorphic: one could declare an instance such as `instance Mutation IO (STRef s) where` even though we intend that the IO monad has exactly one reference type, namely `IORef`.

2. as a result, it is extremely easy to write programs with ambiguous typings, such as:

```hs
readAndPrint :: IO ()
readAndPrint = do
  r <- newRef 'x' -- we must annotate r explicitly
  v <- readRef r
  print v
```

We know, from the type signature, that the computation is performed in the IO monad, but type checker cannot select the type of `r`, since the IO monad could have reference cells of many different types. Therefore, *we must annotate `r` with its type explicitly*. Types are no longer lightweight when they have to be explicitly specified even for such a simple function.

The standard solution to the second problem is to use a *functional dependency*:

```hs
-- monad m determines the reference type r
class Mutation m r | m -> r where
```

> The `m -> r` means that every `m` is related to at most one `r`.

Meanwhile, the main purpose of this paper is to explain an alternative approach in which we express the functional dependency at the type level in an explicitly functional way.

## 2.1 Declaring an associated type

The class `Mutation` does not really have two separate type parameters `m` and `r`; but one type parameter, associated with another type that is functionally dependent. In fact, monad `m` determines the reference datatype `r` (e.g. whether `r` is `IORef` or `STRef`, or some other reference type).

*Type families* allow us to say this directly:

```hs
class Mutation m where
  type Ref m :: * -> *
  newRef   :: a -> m (Ref m a)
  readRef  :: Ref m a -> m a
  writeRef :: Ref m a -> a -> m ()

instance Mutation IO where
  type Ref IO = IORef
  newRef   = newIORef
  readRef  = readIORef
  writeRef = writeIORef

instance Mutation (ST s) where
  type Ref (ST s) = STRef s
  newRef   = newSTRef
  readRef  = readSTRef
  writeRef = writeSTRef
```

The class declaration now introduces a *type function* called `Ref`, here with a specific kind. Each instance declaration must have a clause *defining that type function at the instance type*.

```hs
-- type function declaration
type Ref m :: * -> *

-- type function instantiation at IORef
type Ref IO = IORef

-- type function instantiation at STRef
type Ref (ST s) = STRef s
```

`Ref` is a type family, an associated type, and a type function.

`Ref` is a *type family*, or an *associated type* of the class `Mutation`. It behaves like a function at the type level, so `Ref` is also a *type function*.

> Associated type is a unary type function (unless it is in a MP class)

Applying a type function uses the same syntax as applying a type constructor: `Ref m a` above means to apply the type function `Ref` to `m`, then apply the resulting type constructor to `a`.


The types of `newRef` and `readRef` are now more clearly expressed:

```hs
newRef  :: Mutation m => a -> m (Ref m a)
readRef :: Mutation m => Ref m a -> m a
```

Furthermore, by omitting the functionally determined type parameter from `Mutation`, we avoid the ambiguity problem exemplified by `readAndPrint` above. When performing type inference for `readAndPrint`, the type of `r` is readily inferred to be `Ref IO Char`, which the type checker reduces to `IORef Char`.

In general, now the type checker will reduce
- `Ref IO`     to `IORef`
- `Ref (ST s)` to `STRef s`


These type equalities aside, `Ref` behaves like any other type constructor, and it may be used freely in type signatures and data type declarations.

For example, this declaration is fine:

```hs
data T m a = MkT [Ref m a]
```

## 2.2 Arithmetic

> Associated type is a unary type function. However, inside a MP class it becomes a polyadic type function.

In the `Mutation` class, we used an associated type to avoid a two-parameter type class, but that is not to say that associated types obviate multiparameter type classes.

> By declaring associated types in multiparameter type classes, we introduce type functions that take multiple arguments.

One compelling use of such type functions is to make type coercions implicit, especially in arithmetic.

Suppose we want to be able to write `add a b` to add two numeric values `a` and `b` even if one is an Integer and the other is a Double (and avoiding writing `fromIntegral` explicitly). We also want to add a scalar to a vector represented by a list without writing `repeat` explicitly to coerce the scalar to the vector type. The result type should be the simplest that is compatible with both operands.

We can express this intent using a two-parameter type class, `class Add a b`, whose parameters are the argument types of its `add` method, and whose associated type `SumTy` is the result of the `add` method:

```hs
class Add a b where
  type SumTy a b
  add :: a -> b -> SumTy a b

instance Add Integer Double where
  type SumTy Integer Double = Double
  add x y = fromIntegral x + y

instance Add Double Integer where
  type SumTy Double Integer = Double
  add x y = x + fromIntegral y

instance (Num a) => Add a a where
  type SumTy a a = a
  add x y = x + y
```
