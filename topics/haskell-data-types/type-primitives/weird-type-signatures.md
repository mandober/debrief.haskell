# Weird type signatures

Literal numbers are in fact polymorphic types. 
The type of `2` is `2 :: (Num a) => a`. It is a sort of a type function that awaits for a concrete numeric type, but type application, `2 @Int`, won't work.

This seems like it should work, since it is the application of the `Int` type to the `a` in the `Num a` constraint; that is, the instantiation of the `a` type varible at the `Int` type, which should be fine?


```hs
:t 2
2 :: Num a => a

:t 2 @Int
-- fails

-- wtf?
:t 2 (+)
2 (+) :: (Num a, Num ((a -> a -> a) -> t)) => t

-- wtf!?
:t (5 div 2)
(5 div 2) :: (Integral a, Num t1, Num ((a -> a -> a) -> t1 -> t2)) => t2
```

Lookie, lookie that. Say 5 wtf's and go in pieces.
