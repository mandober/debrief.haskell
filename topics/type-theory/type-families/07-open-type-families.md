Open type families
# Open type families

Let's say we want to assign a textual label to some types, possibly for serialization purposes:

```hs
type Label :: Type -> Symbol
type family Label t where
  Label Double = "number"
  Label String = "string"
  Label Bool   = "boolean"
  -- …
```

We can reify the label at the term level using the `KnownSymbol` class:

```hs
label :: forall t. KnownSymbol (Label t) => String
label = symbolVal (Proxy @(Label t))

ghci> label @Double
"number"
```

But what if the user defines their own type `MyType` in another module? 
How could they assign a label to it, such that `label @MyType = "mt"`?

With closed type families, this is not possible. That is where open type families enter the picture.

**To make a type family open, omit the `where` keyword in its header**

```hs
type Label :: Type -> Symbol
type family Label t
```

The instances are no longer indented, instead they are declared at the top level, possibly in different modules, and prefixed with the `type instance` keyword sequence:

```hs
type instance Label Double = "number"
type instance Label String = "string"
type instance Label Bool   = "boolean"
```

Now a user can easily define an instance of Label for their own type:

```hs
data MyType = MT
type instance Label MyType = "mt"

ghci> label @MyType
"mt"
```

At this point, one might start wondering why anybody would ever prefer closed type families if open type families seem to be more powerful and extensible. 
The reason for this is that extensibility comes at a cost:
> the equations of an open type family are not allowed to overlap; but overlapping equations are often useful!
