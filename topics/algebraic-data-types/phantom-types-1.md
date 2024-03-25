# Phantom types

Motive: we'd like to type-enforce a certain property of a data type (of its values), e.g. we'd like to discern the empty list from nonempty lists, and have it reflected by the type. That way, a function like `head` becomes total; moreover, GHC also understands this, so it doesn't require scrutinizing the case when the list is empty - after all, it cannot be, as announced by the `head` function's (new and improved) type signature.

How: shovelling in the information about the state of a list - whether it is empty or not - can be done naively, by employing more run-time-preserved language constructs. However, the goal here is zero-cost, which means the place where such information should be stored is the type level (coz types are erased by the run-time).

Means: we can sneak this info in using a 2-state phantom type parameter. We can reuse the Bool type, which when lifted makes the kind `Bool` available that has two type ctors, `'True` and `'False`. We can use these two (uninhabited) type ctors as tags - they will act as a flag, each tagging a particular state of a list.

The list type should have an extra type param added `s` (s for state), which is a phantom type used only at the type-level for tagging list values. For now, it may as well be of the `Bool` kind.

```hs
-- List tagged with the "is-empty" status
type La :: Type -> Bool -> Type
data La a (s :: Bool) = Na | Ca a (La a s)

-- The empty list: 'True tags the list as empty
emptyList :: La a 'True
emptyList = Na

-- Non-empty list (of chars): 'False tags the list as nonempty
nonemptyList :: La Char 'False
nonemptyList = Ca 'a' (Ca 'b' Na)
```

Improve: create a general data type that takes (a broad range of types) a type and adds a phantom type param to it.

```hs
-- as the 'data' type
data Tag f a s = Tag f a

-- as the 1-field-only newtype
newtype Tagg f a s = Tagg (f a)
```

However both of these work only with binary type ctors (like `[]`, `NonEmpty`, etc.), [_can we do someting more general?_]. Can it be generalized in the aspect of the tag type: parameterized by the Tag type, so it's not only Bool kind with its two type ctors, but any arbitrary kind? Or at least a system with a specific, even fixed tag, but a system that can add and strip the tag efficiently.

What about the pattern matching? Adding the appropriate *pattern synonyms* would rid us of manual type/kind annotations (specified either as type sigs or TypeApplications).

A more general, binary type ctor tagger:

```hs
-- using this more general, binary type ctor tagger
-- allows us to reue existing data types like [a]
newtype Tagged f a (s :: Bool) = Tagged (f a)
  deriving (Eq)

-- Create the empty list: 'True tags the list as empty
emptyList :: Tagged [] a 'True
emptyList = Tagged []

-- Create a non-empty list: 'False tags the list as nonempty
nonemptyList :: Tagged [] Int 'False
nonemptyList = Tagged [1,2,3]
```
