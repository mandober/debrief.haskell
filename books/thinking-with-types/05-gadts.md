# GADTs

Generalized algebraic datatypes (enabled via *GADTs*) are an extension to the type system that allows annotating data ctors with explicit type signatures, possibly specialized for each data ctor.

The canonical example of GADTs usefulness is a type safe evaluator.

Because GADTs allow us to specify signatures of data ctors, we can use them to constrain a type variable in certain circumstances, which is a feature not possible otherwise.
