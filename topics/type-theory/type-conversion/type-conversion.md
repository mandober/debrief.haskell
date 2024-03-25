# Haskell topics :: Type conversion

Type conversions
- casting (primitives)
- between custom ADTs
- coercion (for newtypes)
  - T    and  newtype NT = NT T
  - Int  and  newtype T  = T Int
- bidirectional conversion
  - isomorphism, isomorphic types, `a ≅ b`
  - *conversion cannot fail under any circumstances!*
  - `From` class, or `Into` class
  - into :: a -> b
  - from :: b -> a
  - from = into⁻¹
  - into = from⁻¹
  - from . into = idᵃ
  - into . from = idᵇ
  - from . into = into . from
  - `String <=> Text` (almost)
    - not quite isomorphic since any Unicode codepoint is a valid `String`, while invalid Unicode codepoints are not valid `Text` (e.g. surrogate pairs, special marking codepoints, nonchars, etc.)
    - `from @String ≈ from @Text`
      (String) |> from @String ==|> (Text)   ==|> from @Text   |> (String)
    - `from @Text ≈ from @String`
      (Text)   |> from @Text   ==|> (String) ==|> from @String |> (Text)
- quasi-isomorhic types
  - *fallible conversion*
  - `TryFrom` class, or `TryInto` class
- unidirectional conversion
  - embedding
    - type A is convertable to B but not vice versa
    - `from :: a -> b` but `¬(from :: b -> a)`
    - Integer (unbounded) is convertable to Int (bounded) with hi-fi,
      but Int is not looslessly convertable to Integer 
      without a potential risk:
      64-bit wide numeric types: Int = Int64, Word = Word64
      - minBound Word =   0
      - maxBound Word =   2^64 - 1
      - minBound Int  = - 2^64
      - maxBound Int  =   2^63 - 1
  - deliberately loosy conversion
    - from `Double`  (d) to `Float` (f)
    - from `Integer` (i) to `Int`   (n)
      - `from :: Integer -> Int`
      maxBound @Word = 2^64 - 1 = ‭18_446_744_073_709_551_616‬
      maxBound @Int  = 2^63 - 1 =  9_223_372_036_854_775_807
      - methods when `result >= maxBound`
        - saturated conversion:
          return maxInt
        - wrap-around (modulo) conversion:
          `from :: Integer -> Int`
          `from i = fromIntegral $ mod i $ fromIntegral $ maxBound @Int`
        - remainder conversion:
          result is implicitly maxBound @Int + remainder
          (which is the same as wrap-around, no?)



## Rust's solution

The `From` trait allows for a type to define how to create itself from another type, hence providing a very simple mechanism for converting between several types.


```rust
pub trait From<T>: Sized {
  /// Converts to this type from the input type.
  fn from(_: T) -> Self;
}

pub trait Into<T>: Sized {
  /// Converts this type into the (usually inferred) input type.
  fn into(self) -> T;
}
```
