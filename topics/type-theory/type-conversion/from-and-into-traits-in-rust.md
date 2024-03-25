# From and Into traits in Rust

Rust copied Haskell's implementation of not only the type system but of many Haskell-specific features. Haskell's classes are Rust's traits but the mechanism is very similar. However, while Rust offers a pair of complementary traits, `From` and `Into`, that provide conversions between the types, Haskell has no similar classes - why the fuck not?

## Rust's `From` trait

https://doc.rust-lang.org/std/convert/trait.From.html

```rs
// Trait std::convert::From
pub trait From<T> {
  // Converts to this type (Self) from the input type (T)
  fn from(T) -> Self;
}
```

`From` is used for value-to-value conversions while consuming the input value. 
`From` is the reciprocal of `Into`.

Prefer implementing `From` over `Into` because `From` automatically provides `Into` due to the *blanket implementation* (trait without methods but with generic types so it "catches" many types) in the standard library.

Only implement `Into` when targeting Rust before 1.41 AND converting to a type outside the current crate. `From` was not able to do these types of conversions in earlier versions because of Rust's *orphaning rules* (see the `Into` trait below for more details).

Generic implementations:
- `From<T>` for `U` implies `Into<U>` for `T`
- `From` is reflexive, so `From<T>` for `T` is implemented


## Rust's `Into` trait

https://doc.rust-lang.org/std/convert/trait.Into.html

```rs
pub trait Into<T> {
  fn into(self) -> T;
}
```

A value-to-value conversion that consumes the input value. 
The opposite of `From`.

Prefer using `Into` over `From` when *specifying trait bounds on a generic function*. This way, types that directly implement `Into` can be used as arguments as well.

Generic implementations:
- `From<T>` for `U` implies `Into<U>` for `T`
- `Into` is reflexive, so `Into<T>` for `T` is implemented


## Implementing traits

In Rust, trait implementation corresponds to Haskell's class implementation.


```rs
// data Wrapper a = Wrapper (Vec a)
struct Wrapper<T>(Vec<T>);

// implementing From for Wrapper
impl<T> From<Wrapper<T>> for Vec<T> {
  fn from(w: Wrapper<T>) -> Vec<T> { w.0 }
}
// ↑↑↑ fails to compile in Rust before 1.4 due to its orphaning rules!

// To get around that, implement Into instead:
impl<T> Into<Vec<T>> for Wrapper<T> {
  fn into(self) -> Vec<T> { self.0 }
}
```


## From and Into

Rust encourages implementing just `From` and getting `Into` for free. However, note that `Into` doesn't provide a `From` implementation (as `From` does with `Into`), so you should always try to implement `From` and then fall back to `Into` if `From` can't be implemented.

Like many others, these two traits are implemented in terms of the "current type", denoted by the keyword `Self` (as the return type in `from`) or `self` (as the input parameter in `into`).

```rs
pub trait From<T> { fn from(T) -> Self; }
pub trait Into<T> { fn into(self) -> T; }

struct Wrapper<T>(Vec<T>);

impl<T> From<Wrapper<T>> for Vec<T> {
  fn from(w: Wrapper<T>) -> Vec<T> { w.0 }
}

impl<T> Into<Vec<T>> for Wrapper<T> {
  fn into(self) -> Vec<T> { self.0 }
}
```

Haskell has no notion of a "current type", so another approach is needed there.

```hs
class From t where
  type Output
  from :: a -> b


class From a b where
  from :: a -> b


class Into a b where
  into :: a -> b


data Wrapper a = Wrapper (Vec a)

instance From (Wrapper a) 

```



### Expanding the args of generic functions

To express that a function accepts any arg as long as it is convertable into some specific type `T`, we can constraint it with `Into<T>`.

```rs
fn generic<T: Into<Vec<u8>>>(s: T) {
  let bytes = b"hello".to_vec();
  assert_eq!(bytes, s.into());
}

let s = "hello".to_string();
generic(s);
```

For example, the `generic` function can take any arg as long as it can be converted into a `Vec<u8>`. Note that `String` implements `Into<Vec<u8>>`.

Rust expresses type constraints by first declaring a type variable, like `T` here given in the angle brackets, right after the function's name. To constrain the type `T`, you can follow it with the colon, `<T: …>`, stating the constraints as traits, `<T: Into<Vec<u8>>>`. So, the type `T` must implement the `Into` trait (or `From`)



`fn generic<T: Into<Vec<u8>>>(s: T) { … }`


**This is exactly what I'd like to express in Haskell...**. That is, expanding the set of types a function works with, by constraining it with an `Into` or `From` class.





## Refs

* Converting numbers
https://wiki.haskell.org/Converting_numbers

* Generic number type
https://wiki.haskell.org/Generic_number_type
