# ConstraintKinds explained on a super simple example

> What is a Constraint kind?

Why would someone use it (in practice)?  

What is it good for? 

Could you give a simple code example to illustrate the answers to the previous two questions?

Why is i...

Well, I'll mention two practical things it allows you to do:

1.  Parametrize a type by a type class constraint
2.  Write type classes that allow their instances to specify constraints that they need.

Maybe it's best to illustrate this with an example. One of the classic Haskell warts is that you cannot make a `Functor` instance for types that impose a class constraint on their type parameter; for example, the `Set` class in the `containers` library, which requires an `Ord` constraint on its elements. The reason is that in "vanilla" Haskell, you'd have to have the constraint on the class itself:

    class OrdFunctor f where
        fmap :: Ord b => (a -> b) -> f a -> f b
    

...but then this class only works for types that require specifically an `Ord` constraint. Not a general solution!

So what if we could take that class definition and abstract away the `Ord` constraint, allowing individual instances to say what constraint they require? Well, `ConstraintKinds` plus `TypeFamilies` allow that:

    {-# LANGUAGE ConstraintKinds, TypeFamilies, FlexibleInstances #-}
    
    import Prelude hiding (Functor(..))
    import GHC.Exts (Constraint)
    import Data.Set (Set)
    import qualified Data.Set as Set
    
    
    class Functor f where
       
       
       type Allowed f :: * -> Constraint
    
       fmap :: Allowed f b => (a -> b) -> f a -> f b
    
    instance Functor Set where
        
        type Allowed Set = Ord
        fmap = Set.map
    
    instance Functor [] where
        
        type Allowed [] = NoConstraint
        fmap = map
    
    
    class NoConstraint a where
    
    
    instance NoConstraint a where
    

(Note that this isn't the only obstacle to making a `Functor` instance to `Set`; see [this discussion](https://www.reddit.com/r/haskell/comments/1njlqr/laws_for_the_eq_class/). Also, [credit to this answer for the `NoConstraint` trick](https://stackoverflow.com/a/29011341/1094403).)

This sort of solution hasn't been generally adopted just yet, though, because `ConstraintKinds` are still more or less a new feature.

* * *

Another use of `ConstraintKinds` is to parametrize a type by a class constraint or class. I'll reproduce [this Haskell "Shape Example" code that I wrote](https://gist.github.com/sacundim/8511f98d6173d8d46533):

    {-# LANGUAGE GADTs, ConstraintKinds, KindSignatures, DeriveDataTypeable #-}
    {-# LANGUAGE TypeOperators, ScopedTypeVariables, FlexibleInstances #-}
    
    module Shape where
    
    import Control.Applicative ((<$>), (<|>))
    import Data.Maybe (mapMaybe)
    import Data.Typeable
    import GHC.Exts (Constraint)
    
    
    
    data Object (constraint :: * -> Constraint) where
        Obj :: (Typeable a, constraint a) => a -> Object constraint
               deriving Typeable
    
    
    
    downcast :: forall a constraint. (Typeable a, constraint a) =>
                Object constraint -> Maybe a
    downcast (Obj (value :: b)) = 
      case eqT :: Maybe (a :~: b) of
        Just Refl -> Just value
        Nothing -> Nothing
    

Here the parameter of the `Object` type is a type class (kind `* -> Constraint`), so you can have types like `Object Shape` where `Shape` is a class:

    class Shape shape where
      getArea :: shape -> Double
    
    
    
    instance Shape (Object Shape) where
        getArea (Obj o) = getArea o
    

What the `Object` type does is a combination of two features:

1.  An existential type (enabled here by `GADTs`), which allows us to store values of heterogeneous types inside the same `Object` type.
2.  `ConstraintKinds`, which allows us to, instead of hardcoding `Object` to some specific set of class constraints, have the users of the `Object` type specify the constraint they want as a parameter to the `Object` type.

And now with that we can not only make a heterogeneous list of `Shape` instances:

    data Circle = Circle { radius :: Double }
                deriving Typeable
    
    instance Shape Circle where
      getArea (Circle radius) = pi * radius^2
    
    
    data Rectangle = Rectangle { height :: Double, width :: Double }
                   deriving Typeable
    
    instance Shape Rectangle where
      getArea (Rectangle height width) = height * width
    
    exampleData :: [Object Shape]
    exampleData = [Obj (Circle 1.5), Obj (Rectangle 2 3)]
    

...but thanks to the `Typeable` constraint in `Object` we can **downcast**: if we correctly guess the type contained inside an `Object`, we can recover that original type:

    
    
    
    
    
    
    example :: [String]
    example = mapMaybe step exampleData
      where step shape = describeCircle <$> (downcast shape)
                     <|> Just (describeShape shape)
    
    describeCircle :: Circle -> String
    describeCircle (Circle radius) = "A Circle of radius " ++ show radius
    
    describeShape :: Shape a => a -> String
    describeShape shape = "A Shape with area " ++ show (getArea shape)


[Source](https://stackoverflow.com/questions/31317159/constraintkinds-explained-on-a-super-simple-example)