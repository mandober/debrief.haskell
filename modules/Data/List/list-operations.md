# List operations

## List are special

Lists is a primitive data type, which the compiler has intimate knowledge of. Among other benefits, this allows lists to have special symbolic names for its parts. At the type level, the name of the type is pronounced list, but the type ctor's name is symbolic, `[]`. It is ok to have symbolic names for type ctors, so the unary list type ctor is declared as `[] a`, which is nothing strange. What is unusual is that it has an alternative, shorthand form, `[a]`. This cannot be normally achived, and it is possible only thanks to the special status of the list type. Writing list types in this shorthand form, e.g. [a] or `[a -> b]` or `[(a, [b])]`, is actually even less clear then e.g. `List a` or `List (a -> b)` or `List (a, List b)` (therefore, some authors often declare a type alias `type List = []`). Moreover, there are situations when you must use the proper, expanded form, `[] a`.


but what is otherwise forbiden for user types is having 
is allowed for lists, i.e. 


unusual is the shorthand variant, i.e. `[] a` = `[a]`, which is forbiden for user-defined data types.

The symbols they use (`[]`, `:`) are reserved symbols.

The type name is `[]` (list), the type ctor is unary, `[] a`, but there is also a shorthand for list type at the type level, `[a]`, which is less clear then using `List a`. At the term level, the proper syntax is `1 : 2 : []`, where `[]` denotes the empty list data ctor, and `:` is cons data ctor. However, there is also term-level shorthand, [1,2].

## List declaration

```hs
data [a] = [] | a : [a]
data []   a = []  | (:)  a [a]
data List a = Nil | Cons a (List a)
```

## List creation
