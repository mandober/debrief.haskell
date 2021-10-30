# Lens

3. Lenses
3.1 Introduction to Lenses

*Optics* allow us to separate concerns, i.e. split up the action we perform on data from the selection of data we want to perform it on. The data selectors are the actual optics, and the operations which can be performed on data are *actions*. Each type of optic comes with a set of compatible actions.

Each type of optic has a different balance constraint vs flexibility, moving to and fro on this spectrum results in several different but useful behaviours. *Lenses* lean closer to the constrained side of things, which means you have a lot of guarantees about their behaviour, but also means that you need to prove those guarantees to make a lens, so there are fewer lenses in the world than there are of the more flexible optics.

Lenses have the following concrete guarantees:
- a Lens focuses (selects) a single piece of data within a larger structure
- a Lens must never fail to get or modify that focus.

These constraints unlock a few actions we can perform on lenses:
- We can use a lens to view the focus within a structure.
- We can use a lens to set the focus within a structure.
- We can use a lens to modify the focus within a structure.
