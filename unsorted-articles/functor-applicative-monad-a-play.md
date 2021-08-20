# Functor, Applicative, Monad, a play

> A blog about functional programming

Posted on July 17, 2019

A dialogue between context and focus, container and containee, computation and value.

    context? focus!   ?!
    context? _        ?
           _ focus!    !

Functor
-------

`Functor`’s `fmap` alters the focus, the context is unchanged.

    fmap alter (context focus) = context (alter focus)   ?!
    fmap alter (context _    ) = context _               ?
    fmap alter (      _ focus) =       _ (alter focus)    !

_The context asks._

    fmap alter (context focus) = context (alter focus)   ?!
    fmap alter (context _    ) = context _               ?
                context        = context                 ?

_The focus answers._

    fmap alter (context focus) = context (alter focus)   ?!
    fmap alter (      _ focus) =       _ (alter focus)    !
         alter          focus  =          alter focus     !

Applicative
-----------

`Applicative`’s `(<*>)` combines foci and contexts simultaneously.

    context1 focus1 <*> context2 focus2 = (context1 <> context2) (focus1 focus2)   ?!
           _ focus1 <*>        _ focus2 =                      _ (focus1 focus2)    !
    context1 _      <*> context2 _      = (context1 <> context2) _                 ?

The `(<>)` of semigroups and monoids is a metaphor. Only a metaphor?

_Foci tell._

    context1 focus1 <*> context2 focus2 = (context1 <> context2) (focus1 focus2)   ?!
           _ focus1 <*>        _ focus2 =                      _ (focus1 focus2)    !
             focus1              focus2 =                         focus1 focus2     !

_Contexts explain._

    context1 focus1 <*> context2 focus2 = (context1 <> context2) (focus1 focus2)   ?!
    context1 _      <*> context2 _      = (context1 <> context2) _                 ?
    context1        < > context2        =  context1 <> context2                    ?

Monad
-----

`Monad`’s `join` nests contexts:

    join (context1 (context2 focus)) = (context1 . context2) focus   ?!
    join (       _ (       _ focus)) =                     _ focus    !
    join (context1 (context2 _    )) = (context1 . context2) _       ?

The `(.)` of functions is a metaphor.

[`(.)` makes a fine `(<>)`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Semigroup.html#t:Endo): every `Monad` is an `Applicative`. Is `(.)` the only `(<>)`?

_A focused mind._

    join (context1 (context2 focus)) = (context1 . context2) focus   ?!
    join (       _ (       _ focus)) =                     _ focus    !
                             focus   =                       focus    !

_A contemplative context._

    join (context1 (context2 focus)) = (context1 . context2) focus   ?!
    join (context1 (context2 _    )) = (context1 . context2) _       ?
          context1 (context2 _    )  = (context1 . context2) _       ?

Divide and conquer
------------------

    data Writer w a = Write w a
    
    div <$> Write "no" 42 <*> Write "thing" 6 = Write "nothing" 9   ?!
    div                42                   6 =                 9    !
                  "no"    < >       "thing"   =       "nothing"     ?

Order and chaos
---------------

    data Maybe b = Nothing | Just b
    
    (>) <$> Just True <*> Just False = Just True   ?!
      _ <$> Just _    <*> Just _     = Just _      ?
            Just          Just       = Just        ?
            Just          Nothing    = Nothing     ?
      join (Just          Nothing)   = Nothing     ?!

Everything and nothing
----------------------

    (^) <$> (\x -> 0) <*> (\x -> x) = (\x -> 0 ^ x)   ?!
    (^)            0             x  =        0 ^ x     !
            (\x ->  )     (\x ->  ) = (\x ->      )   ?


[Source](https://blog.poisson.chat/posts/2019-07-17-functor-play.html)