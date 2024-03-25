# Data vs Control functors

https://www.tweag.io/blog/2020-01-16-data-vs-control/

Haskell's `Data` and `Control` module hierarchies feel arbitrary. There's `Data.Functor` and `Control.Monad`, even though monads are functors. Shouldn't they belong to the same hierarchy? The truth is that there are two different kinds of functors - but you could never tell because they coincide in regular Haskell - data functors and control functors. We can use *linear-types* to show why they are different.

```hs
fmap :: Functor f => (a → b) → (f a → f b)  -- regular Functor
dmap :: Functor f => (a ⊸ b) → (f a ⊸ f b)   -- Data.Functor
cmap :: Functor f => (a ⊸ b) ⊸ (f a ⊸ f b)    -- Control.Functor
```





## Refs

* Data vs Control functors
https://www.tweag.io/blog/2020-01-16-data-vs-control/

* Linear types
https://www.tweag.io/blog/2017-03-13-linear-types/

* Control.Functor.Linear
https://hackage.haskell.org/package/linear-base
https://hackage.haskell.org/package/linear-base-0.4.0/docs/Control-Functor-Linear.html

* Data.Functor
https://hackage.haskell.org/package/base

* Arnaud Spiwack - Data vs Control: a tale of two functors
https://www.youtube.com/watch?v=gm2pK01S8_g

Data vs Control: a tale of two functors
The Haskell base library features the Data and Control module hierarchies. The distinction between data and control is rooted deeply, in computer science: it can already be seen in Turing machines. However, when it comes to Haskell the difference between data and control can be quite muddled. A case in point is functors: there are functor type classes in both hierarchies: Functor and Traversable are in the Data hierarchy, while Applicative and Monad are in the Control hierarchy. This is really confusing! Do functors magically cease to be data and become control structure when I declare a Applicative instance?
In this talk, I will show that there are indeed two kinds of functors, which I dub data functors, and control functors. Each fitting in the respective hierarchy. The catch is that you can't tell them apart in regular Haskell. However, with the new linear types extension, we can see them for what they really are. In fact, when programming with linear types, one typically needs both kinds. I'll be explaining a tiny bit of linear types, just enough to expose the two hierarchies for what they are.
