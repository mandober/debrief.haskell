# Extensible datatypes - HaskellWiki

> Here's a simple test for object orientation (for some reasonable definition):

The problem
-----------

Here's a simple test for object orientation (for some reasonable definition):

Define a type A such that for any type B you can define

up :: B \-> A
down :: A \-> Maybe B

such that

You can do this quite easily in Java or C++, _mutatis mutandis_. You can't do this in Haskell (or [O'Haskell](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/O%27Haskell "O'Haskell") either).

You can do a weaker form of this with Haskell's [Dynamic](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/Dynamic "Dynamic"), where you only have to deal with Bs that are instances of Typeable. But even with that, note that Dynamic/Typeable/TypeRep are a bit messy, with instances for Typeable defined for a wide range of known types.

An alternative approach would be to identify your B within A not per-B but per-(up,down). This would allow for instance separate (up,down) for the same B such that

down1 . up2 \= Nothing
down2 . up1 \= Nothing

Of course this can be done with Dynamic too, by defining dummy types. But it's ugly.

Extensible datatypes
--------------------

**Extensible datatypes** allow a type to be defined as "open", which can later be extended by disjoint union. Here's the Löh-Hinze syntax that achieves the above OO test:

module P where

\-- define open datatype
open data A :: \*

module Q where
import P

\-- add constructor to A
MkB :: B \-> A

up \= MkB
down (MkB b) \= Just b
down \_ \= Nothing

Deriving Dynamic
----------------

It's possible to define [Dynamic](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/Dynamic "Dynamic") using extensible datatypes. Here's a naive attempt:

open Dynamic :: \*

class Typeable' a where
  toDyn :: a \-> Dynamic
  fromDynamic :: Dynamic \-> Maybe a

\-- for each type...

MkBool :: Bool \-> Dynamic

instance Typeable' Bool where
  toDyn \= MkBool
  fromDynamic (MkBool b) \= Just b
  fromDynamic \_ \= Nothing

  
This attempt however doesn't allow easy creation of Typeable1, Typeable2 etc. A better way is to use type-constructor parameters:

open data Dynamic0 :: (\* \-> \*) \-> \*

open data Dynamic1 :: ((\* \-> \*) \-> \*) \-> \*

type Dynamic \= Dynamic0 Identity

data Type a \= MkType

type TypeRep \= Dynamic0 Type

class Typeable0 a where
  toDyn0 :: f a \-> Dynamic0 f
  fromDynamic0 :: Dynamic0 f \-> Maybe (f a)

class Typeable1 p where
  toDyn1 :: g p \-> Dynamic1 g
  fromDynamic1 :: Dynamic1 g \-> Maybe (g p)

data Compose p q a \= MkCompose (p (q a))
data Compose1 d0 f p \= MkCompose1 (d0 (Compose f p))

MkDynamic1 :: (Dynamic1 (Compose1 Dynamic0 f)) \-> Dynamic0 f

unDynamic1 :: Dynamic0 f \-> Maybe (Dynamic1 (Compose1 Dynamic0 f))
unDynamic1 (MkDynamic1 xx) \= Just xx
unDynamic1 \_ \= Nothing

instance (Typeable1 p,Typeable0 a) \=> Typeable0 (p a)
  \-- toDyn0 :: f (p a) -> Dynamic0 f
  toDyn0 \= MkDynamic1 . toDyn1 . MkCompose1 . toDyn0 . MkCompose
  \-- fromDynamic0 :: Dynamic0 f -> Maybe (f (p a))
  fromDynamic0 dyn \= do
    dcdf <- unDynamic1 dyn
    (MkCompose1 dcfp) <- fromDynamic1 dcdf
    (MkCompose fpa) <- fromDynamic0 dcfp
    return fpa

\-- for each type

MkInt :: (f Int) \-> Dynamic0 f

instance Typeable0 Int where
   toDyn0 \= MkInt
   fromDynamic0 (MkInt fi) \= Just fi
   fromDynamic0 \_ \= Nothing

MkMaybe :: (g Maybe) \-> Dynamic1 g

instance Typeable1 Maybe where
   toDyn1 \= MkMaybe
   fromDynamic1 (MkMaybe gm) \= Just gm
   fromDynamic1 \_ \= Nothing

I submit that this is "hairy" rather than "ugly", but I suspect the Type-Constructors Of Unusual Kind (TCOUKs) get even hairier for Typeable2, Typeable3 etc...

Open functions
--------------

_This article is a stub. You can help by expanding it._

References
----------

*   Andres Löh and Ralf Hinze. [Open Data Types and Open Functions](https://www.andres-loeh.de/OpenDatatypes.pdf)


[Source](https://wiki.haskell.org/Extensible_datatypes)