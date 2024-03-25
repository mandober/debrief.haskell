# Function levels

Levels:
1. Level0 is term-level populated by term expressions
2. Level1 is type-level populated by type expressions
3. Level2 is kind-level populated by kind expressions

At *compile-time*, all 3 levels exist; type-level is populated with types that classify the terms, and kind-level is populated with kinds that classify the types. Even in Dependent Haskell that has the `TypeInType` axiom which equalizes types and kinds, types still get classified by kinds; they may be the same entity underneath, but there is still a notion of types and kinds.

At *run-time* there are only terms; types and kinds have served their purpose, so they are erased after a successful compilation.

There are 4 flavours of functions considering term-level and type-level:
1. `term` ⟼ `term`: ordinary functions
2. *Type* ⟼ `term`: classes
3. *Type* ⟼ *Type*: type families
4. `term` ⟼ *Type*: GADTs (partially)
