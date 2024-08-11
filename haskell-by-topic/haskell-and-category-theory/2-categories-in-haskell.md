# Haskell :: Haskell and CT :: Categories in Haskell

Haskell gives rise to a category called `Hask` where Haskell *types* are objects and Haskell *functions* are morphisms.

While it is possible to define other categories in Haskell, they are rarely mentioned. For example, another category could be made of Haskell types as objects and Haskell linear functions as morphisms. It seems, when discussing CT in Haskell, we are only ever considering the `Hask` category.

In CT, given any category, there is also the notion of the subcategories of that category, the category opposite to it, and a Kleisli category based on it. Subcategories of Hask (whatever they may be) do exists and are mentioned, but it is unclear what the category opposite of Hask would be, or the Kleisli category based on it.

- category: `Hask`
- objects: Haskell types
- morphisms: Haskell functions
