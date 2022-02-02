---
downloaded:       2022-01-01
page-url:         https://wiki.haskell.org/Partibles_for_composing_monads
page-title:       Partibles for composing monads - HaskellWiki
article-title:    Partibles for composing monads - HaskellWiki
---
# Partibles for composing monads - HaskellWiki

Having praised monads to the hilt, let me level one criticism. Monads tend to be an all-or-nothing proposition. If you discover that you need interaction deep within your program, you must rewrite that segment to use a monad. If you discover that you need two sorts of interaction, you tend to make a single monad support both sorts. It seems to me that instead we should be able to move smoothly from no monads (no interactions) to one monad (a single form of interaction) to many monads (several independent forms of interactions). How to achieve this remains a challenge for the future.
Having praised monads to the hilt, let me level one criticism. Monads tend to be an all-or-nothing proposition. If you discover that you need interaction deep within your program, you must rewrite that segment to use a monad. If you discover that you need two sorts of interaction, you tend to make a single monad support both sorts. It seems to me that instead we should be able to move smoothly from no monads (no interactions) to one monad (a single form of interaction) to many monads (several independent forms of interactions). How to achieve this remains a challenge for the future.

[How to Declare an Imperative][1], Philip Wadler.

Assuming the [partible][2] types being used are appropriately defined, then:

instance Partible a \=> Monad ((\->) a) where
    return x \= \\ u \-> case part u of !\_ \-> x

    m \>>= k  \= \\ u \-> case part u of
                        (u1, u2) \-> case m u1 of !x \-> k x u2

    m \>> w   \= \\ u \-> case part u of
                        (u1, u2) \-> case m u1 of !\_ \-> w u2

    fail s   \= \\ u \-> case part u of !\_ \-> error s

Furthermore:

instance (Partible a, Monad ((\->) a)) \=> MonadFix ((\->) a) where
    mfix m \= \\ u \-> yet (\\ x \-> m x u)

instance (Partible a, Monad ((\->) a), Partible b, Monad ((\->) b)) \=> MonadCommute ((\->) a) ((\->) b) where 
    mcommute g \= \\ v u \-> g u v

instance (Monad m, Partible b, Monad ((\->) b)) \=> MonadCommute m ((\->) b) where
    mcommute m \= \\ v \-> liftM ($ v) m

where:

yet   :: (a \-> a) \-> a
yet f \=  f (yet f)

class Monad m \=> MonadFix m where
    mfix :: (a \-> m a) \-> m a

class (Monad m1, Monad m2) \=> MonadCommute m1 m2 where
    mcommute :: m1 (m2 a) \-> m2 (m1 a)

Using partible types to define monadic ones can enable an intermediate approach to the use of effects, in contrast to the *all-or-nothing proposition* of only using the monadic interface. In addition, if the definitions for such monadic types are *visible* (e.g. as type synonyms), this may also allow the manipulation of control in ways beyond what the monadic interface provides.

### Composing arrows

Partible types can also be used to define [arrow][3] types:

type A p b c \= (p \-> b) \-> (p \-> c)

arr :: Partible p \=> (b \-> c) \-> A p b c
arr f \= \\ c' u \-> f $! c' u

infixr 3 \*\*\*
(\*\*\*) :: Partible p \=> A p b c \-> A p b' c' \-> A p (b, b') (c, c')
f' \*\*\* g' \= \\ c' u \-> let !(u1:u2:u3:\_) \= parts u in
                      let !(x, x')      \= c' u1 in
                      let !y            \= f' (unit x) u2 in
                      let !y'           \= g' (unit x') u3 in
                      (y, y')                           
            where
              unit x u \= case part u of !\_ \-> x

(...though most will probably opt for the convenience of the associated [`Kleisli`][4] type).

---

See also:

-   [Plainly partible][5]
-   [Partible laws][6]
-   [Burton-style nondeterminism][7]
-   [MonadFix][8]
-   [Prelude extensions][9]
-   [Bang patterns][10]
-   [GHC ticket 19809: Overhaul ST using *pseudodatata*][11]

  
[Atravers][12] 04:31, 10 April 2018 (UTC)

[1]: https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.91.3579&rep=rep1&type=pdf
[2]: https://wiki.haskell.org/Partible "Partible"
[3]: https://wiki.haskell.org/Arrow "Arrow"
[4]: https://hackage.haskell.org/package/base-4.15.0.0/docs/src/Control-Arrow.html#Kleisli
[5]: https://wiki.haskell.org/Plainly_partible "Plainly partible"
[6]: https://wiki.haskell.org/Partible_laws "Partible laws"
[7]: https://wiki.haskell.org/Burton-style_nondeterminism "Burton-style nondeterminism"
[8]: https://wiki.haskell.org/MonadFix "MonadFix"
[9]: https://wiki.haskell.org/Prelude_extensions "Prelude extensions"
[10]: https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/bang-patterns.html
[11]: https://gitlab.haskell.org/ghc/ghc/-/issues/19809
[12]: https://wiki.haskell.org/User:Atravers "User:Atravers"
