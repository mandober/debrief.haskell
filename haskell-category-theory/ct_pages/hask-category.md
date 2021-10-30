# Category theory and Haskell

A category consists of set of objects, a set of morphisms between the objects, and two axioms: every object has the identity arrow, and morphisms are closed under composition. We write `f : A → B` for morphism `f` that goes from the object `A` as the source, to the object `B` as the target. Every object `A` in a category `C` must have the identity morphism, `Iᴀ : A → A`. That morphisms are closed under composition means that, if there's a morphism `𝓯 : A → B` and `𝓰 : B → C`, then there must be a morphism `𝓱 = 𝓯 ◦ 𝓰 : A → C`, which is a composition of arrow `f` after arrow `g`.


## Hask category

https://wiki.haskell.org/Hask

* 𝗛𝗮𝘀𝗸 is the category consisting of Haskell types and functions. Not. The problem is that laziness and bottom value make all the categorical the requirements fail. Because of that, Haskell enthusiast tend to think in some subset of Haskell types where types rid themselves of bottom somehow. Such a language subset only includes functions that terminate and have only finite values. In such a setting, the Hask' category does indeed have the initial and terminal objects as expected, as well as sums and products, and instances of Functor and Monad really are endofunctors and monads.

* Therefore, ignoring bottom, Haskell types form a category 𝗛𝗮𝘀𝗸 where the objects of Hask are Haskell types, and the morphisms from objects A to B are Haskell's functions of type `a -> b`. The identity morphism for object/type `A` is `id :: A -> A`, and the composition of morphisms f and g is `f . g = \x -> f (g x)`.
