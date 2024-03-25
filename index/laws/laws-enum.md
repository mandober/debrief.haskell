

For any type that is an instance of class Bounded as well as Enum , the following should hold:

The calls succ maxBound and pred minBound should result in a runtime error.

fromEnum and toEnum should give a runtime error if the result value is not representable in the result type. For example, toEnum 7 :: Bool is an error.

enumFrom and enumFromThen should be defined with an implicit bound, thus:

   enumFrom     x   = enumFromTo     x maxBound
   enumFromThen x y = enumFromThenTo x y bound
     where
       bound | fromEnum y >= fromEnum x = maxBound
             | otherwise                = minBound
