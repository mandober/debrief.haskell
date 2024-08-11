# Heterogeneous list

Native Haskell list only supports homogeneous elements, i.e. all elements must have the same type. However, data ctor promotion enables heterogeneous list (hlist), which has representation at both term and type level and they both need to be "in sync".

The plain list type:

```hs
-- native list
data [a] = [] | a : [a]

-- user-defined list
data List a = Nil | Cons a (List a)
```

The hlist type (courtesy by the data ctor promotion):

```hs
type data [a] = [] | a : [a]
```

The two data ctors are promoted to type ctors, although both classify no types, i.e. both are uninhabited type ctors, but now they can serve as new kinds.

```hs
>>> :kind '[]  :: forall k. [k] -- here we must use the tick
>>> :kind '(:) :: forall k. k -> [k] -> [k]

-- the list type ctor keeps its usualy kind:
>>> :kind [] :: Type -> Type
```

If we had a heterogeneous list, its type/kind would be [Type] or `List Type` since it can contain various `Type`s.
- list is [Type]
- nil  is '[]
- cons is '(:)

So the empty HList would be unique and its unique type would be: 
- `[]` plain empty list at term level
- `[a]` plain empty list at type level
- i.e. `[] :: forall a. [a]`

```hs
>>> :kind! '[]
'[] :: forall a. [a]
= '[]

>>> :kind! '[Int]
'[Int] :: [Type]
= (:) @Type Int ('[] @Type)
= (:) Int ('[])
= Int ': '[]

>>> :kind! Bool ': Int ': '[]
Bool ': Int ': '[] :: [Type]
= (:) @Type Bool ((:) @Type Int ('[] @Type))
= (:) Bool ((:) Int ('[]))
= Bool : (Int : ('[]))
= Bool : Int : '[]
```

Just like with regular lists
- `'[]`          is desugared into `'[]`
- `'[Bool]`      is desugared into `Bool ': '[]`
- `'[Bool, Int]` is desugared into `Bool ': Int ': '[]`

~~So why would we define a custom HList type when the data ctor promotion does it automatically for us? Because that only gives us the type level syntax for hlists, but we also need the term level syntax and we cannot just use the good ol' regular list in hetero settings.~~






## Heterogeneous list

HList can represent sequences of values of different types.

HList works and has representation on both term and type level, and both levels must be synced. When list data ctors, nil = [] and cons = (:), are promoted to types, they have the kinds:
  >>> :kind '[]  :: forall a. [a]
  >>> :kind '(:) :: forall a. a -> [a] -> [a]
while the list ctor has its usualy kind:
  >>> :kind []   :: Type -> Type


Let's first recall the type of plain lists, in the native and user flavours, for comparison:


1. At the type level:

  data [a] = '[] | a ': [a]


  >>> :k! Char ': Bool ': '[]
  Char ': Bool ': '[] :: [Type]
  = (:) @Type Char ((:) @Type Bool ('[] @Type))


  list = [Type]
  nil  = '[]
  cons = '(:)

2. At the term level

  data List a = Nil | Cons a (List a)
