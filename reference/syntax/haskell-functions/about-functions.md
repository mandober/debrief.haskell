# Functions

- function
- noraml, reducable functions
- irreducable, tagging functions (data ctors)
- field accessors (data ctor inverses)
- parametric polymophism
- ad-hoc polymorphism

With functios being the most important language construct, it's no surprise there are a few distinct flavours of functions in Haskell. SOme of these flavours include:
- reducable functions (regular functions)
- tagging functions (data constructors)
- field accessors

**Reducable functions** are the usual functions, common to many PL. They take an argument and perform a calculation, returning the result that is in a way in a reduced form. For example, the identity function is a reducable function, although it's a bit special since it returns the arg as it takes it, but the increment function takes an integer and adds 1 to it. So, e.g. it may take 2 and add 1 to it, obtaining the expression 2+1, which is reducable to the term 3.

**Tagging functions** are actually *data constructors*. Data ctors are functions as well as their types confirm, but they are special because they don't reduce their argument. Instead, they take an arg and "tag" it. This tag is a peculiar language construct known as a *label*. In fact, each data ctor's name is such a label, and a data ctors jsut slaps that name onto an argument it receives. For example, Peano numbers are defined using a sum type with two data ctors, `Zero` and `Succ`. The data ctor `Zero` is a constant function, it's got nothing to apply itself to, so it just returns the symbol `Zero`. The `Succ` function does take an arg, but unlike function that reduce their args, it just slaps the symbol `Succ` to whatever arg it receves; it tags the argument with another `Succ` label. If the arg it receives is `Zero`, it returns `Succ Zero`, which is an irreducable expression.

**Field accessors** are a part of an ADT type declaration. They are also functions, albeit special because they have no explicit definition. In this example, `unTag` is such a function, i.e. a field accessor. It accesses and retrives the value of the field that is identified by the type stated after the `::` symbol; in this case, the value retreved is of type `a`.

```hs
newtype Ty a = Tag { unTag :: a }

-- the type of unTag
unT :: Ty a -> a
-- the definition of unTag, were it given explicitly:
unT (Tag a) = a
```

A field accessor is a special kind of function because it has an implicit definition, albeit a simple one. Such a function is always applied to the type the declaration of which it is part. In the case of `unTag`, it is applied to the type `Ty a`, the value of which is `Tag a`, and it returns a value of type `a`, after "stripping" the data ctor label, i.e. `Tag`, from the arg. In this way, a field accessor is a kind of inverse to a data ctor: a data ctor function tags a value, a field accessor function untags it.


Functions have *monomorphic type* when they are over the base types (e.g. `Int -> Int`) or *polymorphic type* when they contain type variables that are pretty much always universally quantified, making this polymorphic types *polytypes*.

Functions are parameterized, usually by an input paramater (formally, one input, one output value), but functions may also be parameterized by their return type.
