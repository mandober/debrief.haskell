# Comonad laws

<!-- TOC -->

- [extract and extend](#extract-and-extend)
- [fmap, extract and duplicate](#fmap-extract-and-duplicate)
- [duplicate and extend](#duplicate-and-extend)

<!-- /TOC -->

The applicable set of Comonad laws depends on the exact definition of Comonad, since there are two distinct ways to define it, along with the third that encompasses both.


## extract and extend

1. Provide definitions for `extract` and `extend` satisfying these laws:

```cpp
extend extract      = id
extract . extend f  = f
extend f . extend g = extend (f . extend g)
```

In this case, you may simply set `fmap = liftW`

These laws are directly analogous to the laws for Monads and perhaps can be made clearer by viewing them as laws stating that *Cokleisli composition must be associative, and has `extract` for a unit*:

```cpp
f =>= extract   = f
extract =>= f   = f
(f =>= g) =>= h = f =>= (g =>= h)
```



## fmap, extract and duplicate

2. Provide definitions for `fmap`, `extract` and `duplicate` satisfying laws:

```cpp
extract      . duplicate = id
fmap extract . duplicate = id
duplicate    . duplicate = fmap duplicate . duplicate
```

In this case, you may not rely on the ability to define `fmap` in terms of `liftW`.




## duplicate and extend

3. You can define both `duplicate` and `extend`, in which case you must also satisfy these laws:

```cpp
extend f  = fmap f . duplicate
duplicate = extend id
fmap f    = extend (f . extract)
```

These are the default definitions of `extend` and `duplicate` and the definition of `liftW` respectively.
