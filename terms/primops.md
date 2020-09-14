# primops

https://www.fpcomplete.com/haskell/tutorial/primitive-haskell/

Peeling back some of the layers of abstraction in Haskell coding to understand things like primitive operations, evaluation order, mutation.

Dealing with addition, like `1+2` is more complicated because the number literals are polymorphic so we better start with `1 + 2 :: Int` and the resulting process that transpires in GHC.

- the `Num` class:
  - https://www.stackage.org/haddock/lts-1.0/base-4.7.0.2/Prelude.html#t:Num
- the `Num Int` instance:
  - https://www.stackage.org/haddock/lts-1.0/base-4.7.0.2/src/GHC-Num.html#Num
- `data Int = I# Int#`
  - https://www.stackage.org/haddock/lts-1.0/ghc-prim-0.3.1.0/GHC-Types.html#t:Int
- `+# :: Int# -> Int# -> Int#`
  - https://www.stackage.org/haddock/lts-1.0/ghc-prim-0.3.1.0/GHC-Prim.html#v:-43--35-



```hs
-- addition is polymorphic as are the number literals
1 + 2
-- addition with concrete types
1 + 2 :: Int

-- Since (+) is a method of the Num class
-- we need the `Num Int` instance declaration:
instance Num Int where
    I# x + I# y = I# (x +# y)

-- I# is a data ctor of the Int AlgDT
data Int = I# Int#
-- and +# 
+# :: Int# -> Int# -> Int#
```
