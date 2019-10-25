# About Haskell

Haskell    
Haskell is a purely-functional programming language named after the logician Haskell Curry. Filename extensions are .hs and .lhs (literate haskell). Official Website is www.haskell.org.

Functional    
Haskell is declarative and functional language. Everything is done through functions. Functions are first-class data values and the compiler checks the types of functions just like those of any other data. All functions come with the free auto-currying support.

Purity     
Functions are side-effects free, they can be memoized.

Mutability    
Haskell maintains referential transparency, there is no mutation - once initialized a variable keeps its value.

Lazy      
Nothing is done until really needed. The arguments are not evaluated when the function is applied, only when/if the arguments are actually used.

Compiled or interpreted     
Haskell's main implementation is de facto Glasgow Haskell Compiler (GHC). Besides compilation, it can also act as an interpreter for interaction with Haskell in the REPL.

Static typing    
Type-checking is performed at compile time.

Type inference    
Type annotations are optional as the compiler has full type-inference capability, courtesy of Hindley-Miner typing system.

Type safety     
The compiler performs strict type checking, disallowing operations if the types are not compatible. Moreover, the strictness means that implicit type casts and type coercions are either disallowed or reduced to a minimal set of fully documented, well-known and largely expected exemptions (e.g. coercing an Int into a Float).

Pattern matching    
Pattern matching provides a means of selecting different computations depending on the structure of the data. It is succinct with a minimum of ceremony.

Type classes    
Introduced in Haskell, type classes are like traits or interfaces in that they declare methods an instance type must implement.

Category theory     
Haskell is heavily influenced by mathematics, especially abstract algebra and category theory. Monads are used as means to abstract a pattern of computation.
