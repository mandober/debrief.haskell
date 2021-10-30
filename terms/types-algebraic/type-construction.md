# Type Construction

Construction of user-defined types in Haskell.

## Algebraic type construction

Algebraic type construction relates the concepts from algebra to the choir of constructing a custom type.

The algebraic notion of
- `0` corresponds to the empty data type, `Void`
- `1` corresponds to the unital type, `()`
- `+` corresponds to the sum type, `a | b` (logical AND)
- `⨯` corresponds to the product, `(a, b)` (logical OR)

```hs
data O
data I = I
data Sum a b = Tag1 a | Tag2 b
data Product a b = Product (a, b)   ≅   data Product a b = Product a b
```


The two basic mechanism form constructing a new type are sum and product algebraic types.

In general, any type can be represented as **a possibly recursive sum of products**.


The components of a sum type are called *alternants* or *variants*, and each one is distinguished by its own tag (data ctor). The part about tagging is the reason why this type construct is also called *tagged disjoint union*; "disjoint" is closely related to tagging and means that even if two variants have the same type, their respective values can still be distinguished by their unique tag.

For example, `Left True` vs `Right True` values of the `Either Bool Bool` type.

A sum type like `Either`, having two alternants both of the same type like `Bool`, does not hold a value like `True` unwrapped (untagged); that is, it is always possible to tell, by pattern-matching the tag, which alternant really holds some value. You could say, a value is tainted by the tag, bearing the mark of its distinct data constructor.


In a product type the components are called *fields* and they are accessed by the position; or by using the *record syntax*, which offers field access, field update, record construction by field names, partial record update based on defaulting the unspecified fields on another record.
