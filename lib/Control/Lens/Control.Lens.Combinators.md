# Control.Lens.Combinators

```hs
-- This lets the subset of users who vociferously disagree about the full
-- scope and set of operators that should be exported from lens to not have
-- to look at any operator with which they disagree.
--
-- > import Control.Lens.Combinators
module Control.Lens.Combinators (..) where

import Control.Lens hiding
  ( (<|)    , (|>)
  , (^..)
  , (^?)    , (^?!)
  , (^@..)  , (^@?)   , (^@?!)
  , (^.)    , (^@.)
  , (<.)    , (.>)    , (<.>)
  , (%%~)   , (%%=)
  , (&)     , (&~)    , (<&>)
  , (??)
  , (<%~)   , (<+~)   , (<-~)   , (<*~)
  , (<//~)  , (<^~)   , (<^^~)  , (<**~)
  , (<||~)  , (<&&~)  , (<<%~)  , (<<.~)
  , (<<+~)  , (<<-~)  , (<<*~)  , (<<//~)
  , (<<^~)  , (<<^^~) , (<<**~) , (<<||~)
  , (<<&&~) , (<<<>~) , (<%=)
  , (<+=)   , (<-=)   , (<*=)  , (<//=)
  , (<^=)   , (<^^=)  , (<**=) , (<||=)
  , (<&&=)  , (<<%=)  , (<<.=) , (<<+=)
  , (<<-=)  , (<<*=)  , (<<//=), (<<^=)
  , (<<^^=) , (<<**=) , (<<||=), (<<&&=)
  , (<<<>=) , (<<~)   , (<<>~) , (<<>=)
  , (<%@~)  , (<<%@~) , (%%@~) , (%%@=) , (<%@=) , (<<%@=)
  , (.@=)   , (.@~) 
  , (^#)  , (#~)  , (#%~)  , (#%%~)  , (#=)  , (#%=)  , (<#%~)
  , (<#%=)  , (#%%=)  , (<#~)  , (<#=)  , (...)  , (#)
  , (%~) , (.~)  , (?~)  , (<.~)  , (<?~)  , (<<?~)  , (<<?=)
  , (+~)  , (*~)  , (-~)  , (//~)  , (^~)  , (^^~)  , (**~)
  , (||~)  , (&&~)  , (.=)  , (%=)  , (?=)  , (+=)  , (-=)
  , (*=)  , (//=)  , (^=)  , (^^=)  , (**=)  , (&&=)  , (||=)
  , (<~)  , (<.=)  , (<?=)  , (<>~)  , (<>=)  , (%@~)  , (%@=)
  , (:>)  , (:<)
  )
```