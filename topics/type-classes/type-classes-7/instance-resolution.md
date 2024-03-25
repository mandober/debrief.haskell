# Instance resolution

An instance declaration has the following shape:

```hs
instance <context> => C <ty>
```

The part to the left of the `=>` is called **instance context**, while the part to the right is called the **instance head**. The entire thing is usually called the **instance header**.

When GHC attempts to determine which instance to pick to resolve a class constraint, *it only looks at the instance heads*, completely ignoring the instance contexts at that time. Only once the GHC commits to its choice, does it consider the instance context.

## Explanation with an example

Given the definition of `HList`, suppose we want to generate a `HList` full of `()` values of arbitrary length using a class.

```hs
-- | Class for generating a HList full of units.
class UnitList as where
  unitList :: HList as

-- | Base case: empty type list means the empty hlist is generated.
instance UnitList '[] where
  unitList :: HList '[]
  unitList = HNil

-- | Base case: nonempty type list means generate hlist with as many unit values as there are types (i.e. () types) in the type-level list. It won't diverge because the type will constrain the number of list items.
instance UnitList as => UnitList (() ': as) where
  unitList = () `HCons` unitList

-- The type, [(), (), …], dictates the number of unit values in the hlist.
x1 :: HList '[(), (), ()]
x1 = unitList -- () `HCons` () `HCons` () `HCons` HNil

-- Now suppose we write a function that only accepts a HList containing exactly one unit element, which it returns:
unsingleton :: HList '[a] -> a
unsingleton (x `HCons` HNil) = x

-- but evaluating this fails
x2 = unsingleton unitList

  error:
  • Ambiguous type variable 'a0' arising from a use of 'unitList'
    prevents the constraint '(UnitList '[a0])' from being solved.
    Probable fix: use a type annotation to specify what 'a0' should be.
    These potential instances exist:
      instance UnitList as => UnitList (() : as)
```

The type error says that `a0` is ambiguous, but it only lists a single matching `UnitList` instance - the one we want - so, how does the instance selection create ambiguity? When we wrote this instance:

```hs
instance UnitList as => UnitList (() ': as) where
```

we said that the first element of the type-level list must be `()`, so there's nothing stopping someone from coming along and defining an instance that requires the first element to be an `Int`:

```hs
instance UnitList as => UnitList (Int ': as) where
  unitList = 0 `HCons` unitList
```

In that case, GHC would know which instance to pick.

Nothing in the type of `unsingleton` forces the element in the list to have type `()`, so both instances would be equally valid. It doesn't matter that the instance involving Int doesn't exists at the moment - because classes are open, GHC must take into account that such an instance could be defined.

To hedge against this potential possibility, GHC rejects the program as ambiguous from the get go.

However, we can force GHC to pick our instance using the equality constraint trick. The instance we've defined has the context `UnitList as` which we'll augment with an equality constraint. Here it is again for comparison:

```hs
instance (UnitList as) => UnitList (() ': as) where
  unitList = () `HCons` unitList
```

W augment its context with the equality constraint, `a ~ ()`, which will remove the ambiguity when the expression `unsingleton unitList` is evaluated.

```hs
instance (a ~ (), UnitList as) => UnitList (a ': as) where
  unitList = () `HCons` unitList
```

We first have to modify the head into `UnitList (a ': as)` so that we have an explicit type variable, `a`, in place of the hardcoded `()` type. Having the type variable matches any type (not just `()`) during instance selection, plus it allows us to attach the equality constraint `a ~ ()`.

Although the original and the modified instances seem identical, they act differently. After GHC picks an instance, it will considered the context, and the context will than force the type variable `a` to unify with the `()` type.


>That's the trick: replace the concrete type `T` with a type variable `a` in the instance head, and place the equality constraint `a ~ T` in the context. Now, the type variable `a` in the head make for an infallible type match, thereby making sure the instance is selected. Once it is selected, the equality constraint `a ~ T` in the instace context will kick in, forcing the type variable `a` to unify with the type `T`. Logically, it amounts to the same, but practically, due to the way GHC resolves instances, the trick makes a big difference.

This explains why the two `UnitList` instances behave differently:
* Given the instance head `UnitList (() ': as)`, GHC won't select the instance unless it knows the first element of the type list is `()`. And GHC cannot be sure about this since classes are open and someone may potentially define a conflicting instance.
* But given the instance head `UnitList (a ': as)`, GHC picks the instance *regardless of the type of the first element* - all that matters is that the type list has at least one element.


In practice, this is very useful because it gives the direct control over the type inference process to the programmer.
* When you put a concrete type in the instance head, you're asking GHC to figure out how to unify the types. When you want the types to inform which instance is picked that is very desirable.
* But when you do "the trick", you are actually taking the responsibility of type unification on yourself (you're saying to GHC "you don't tell me, I'll tell you what type this is"), effectively taking a part in the type inference process.

From this perspective, *instances with equality constraints make GHC's type inference algorithm extensible*. You get to pick which decisions are made and when, and crucially, you can use knowledge of your own program structure to expose more information to the typechecker.
