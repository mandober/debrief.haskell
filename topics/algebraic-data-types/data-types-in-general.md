# Haskell Debrief :: Syntax :: Data types in general

Data types in general, meaning considered as algebraic data types, are split by the manner of their construction into:
- the empty (zero) type
- the unit (one) type
- sum type
- product types
- exponential types (functions)

The zero type is the easiest one to describe: it is an empty, completely unihabited type, so there are no values of the empty type. In Haskell, this type is represented by the type `Void`, which contains (almost) no values (no type in Haskell is empty because the value bottom, that marks divergence, inhabits each type; however, at lower levels from the surface syntax, live unlifted types that are indeed bottom-less). Anyway, we ignore bottom, taking the "fast and loose" amendmant, so `Void` is our empty type. Having the empty type is not interesting per se, but it comes in handy like the zero number does.


The two most common ways under which the construction of data types is analyzed are sums and products. Both products and sums construct compound types, meaning both are composed of *multiple types, i.e. their component types*. Products and sums are not types per se, but construction classes; they classify the two most common ways to construct types.

Product types (like tuples and records) require the presence of all the values of their component types in order to produce a valid value of the overall product type.

Sum types (like unions and enums) require the presence of at least one value, whose type is one of the component types, in order to produce a valid value of the overall sum type.


Product types correspond to logical conjunction (AND) so they require all their components to produce the overall type/value.

Sum types correspond to logical disjunction (OR) so they describe a compoud data type, but need only one of the components to produce the overall type/value.

Product types are implemented in PLs are records, structs, tuples, but their canonical form is a pair, `a ⨯ b`, stemming from the Cartesian product of sets.

Sum types are implemented in PLs are unions, enums, but their canonical form is a disjoint pair, `a + b`, stemming from the disjoint union of sets.

Product types consists of multiple subtypes (but not in the subtyping sense), i.e. they consists of any number of subtypes, called **fields**. In fact, they must have at least one field, otherwise they would be isomorphic to the unit type.

```hs
-- decl of a product type with 4 fields
data Product = Product (Int Double Char String)

-- a value of the product type
p1 :: Product
p1 = Product 1 pi 'x' "con"

-- the fields are actually args
data Prod where
  Prod :: Int -> Double -> Char -> String -> Prod
```


Sum types also consists of multiple components, each called a **variant**. In fact, each variant can be considered as a **branch** because they also serve to branch out computation in e.g. functions.
