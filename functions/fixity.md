# Fixity

- There are 3 infix related keywords: infixl | infix | infixr
- infix keywords define fixity i.e. precedence and associativity
- associativity is left (infixl) or right (infixr), or it is neutral (infix)
- precedence is defined by the 0..10 scale (lo-hi)
- The biggest precedence (10) is reserved for function application


infixl 10 (function application)
infixr 9  .
infixr 8  ^, ^^, **
infixl 7  *, %, /, `quot`, `rem`, `div`, `mod`
infixl 6  +, -
infixr 5  ++
infixl 4  <$, <*>, <*, *>, <**>
infixl 1  >>=, >>
infixr 1  =<<
infixr 0  $, $!



infixr
======

a -> b -> c = a -> (b -> c)
a $  b    c = a $  (b    c)
a .  b .  c = a .  (b .  c)
a ^  b ^  c = a ^  (b ^  c)
a ++ b ++ c = a ++ (b ++ c)

infixl
======
a + b + c = (a + b) + c
a   b   c = (a   b)   c
