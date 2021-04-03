# Category theory and Haskell


A category consists of:
* a set of objects
* a set of morphisms between the objects
* a set of upheld axioms
  - every object has the identity arrow
  - morphisms are closed under composition

We write `f : A → B` for morphism `f` that goes from the object `A` as the source, to the object `B` as the target.

Every object `A` in a category `C` must have the identity morphism, `Iᴀ : A → A`

Morphisms are closed under composition means that 
if there is a morphism `𝓯 : A → B` 
and a morphisms `𝓰 : B → C` 
then there must the morphism `𝓱 = 𝓯 ◦ 𝓰 : A → C` 
that is, a composition of arrow f after arrow g.


## Ref

Hask category @Haskell wiki
https://wiki.haskell.org/Hask

Haskell's category-theoretic properties @nlab
https://ncatlab.org/nlab/show/Haskell

Haskell and category theory @wikibooks
https://en.wikibooks.org/wiki/Haskell/Category_theory

Hask is not a category - Andrej Bauer
http://math.andrej.com/2016/08/06/hask-is-not-a-category/

Does it matter if Hask is (not) a category?
https://ro-che.info/articles/2016-08-07-hask-category

Hussling Haskell types into Hasse diagrams
http://blog.ezyang.com/2010/12/hussling-haskell-types-into-hasse-diagrams/

Category Theory 4.2 Products - Bartosz Milewski
https://www.youtube.com/watch?v=Bsdl_NKbNnU&list=PLbgaMIhjbmEnaH_LTkxLI7FMa2HsnawM_&index=8

https://en.wikibooks.org/wiki/Haskell/Category_theory
https://wiki.haskell.org/Category_theory
https://wiki.haskell.org/Hask
