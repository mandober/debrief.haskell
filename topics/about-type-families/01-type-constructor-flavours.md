# Type constructor flavours

There are several categories to which a given type constructor `T` may belong:
- GADT,                `data T a b where …`
- data type, ADT,      `data T a b = …`
- newtype           `newtype T a b = …`
- type class,         `class C t where …`
- type synonym,        `type T a b = …`
- type family,  `type family T a b where …`
- data family,  `data family T a b = …`

The TypeFamilies extension introduces the last two extra categories.


* There are several categories to which a type constructor `T` may belong:

```hs
type    T a b         -- type synonym
newtype T a b         -- newtype
data    T a b         -- data type
class   T a b         -- type class
```

* The `TypeFamilies` extension introduces two more:

```hs
type family T a b     -- type family
data family T a b     -- data family
```
