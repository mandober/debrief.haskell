# 4. Declarations and Bindings

The syntax and informal semantics of Haskell declarations.

```hs
module
 := module modid [ exports ] where body
  | body

body
 := { impdecls ; topdecls }
  | { impdecls }
  | { topdecls }

topdecls := topdecl1 ; … ; topdecln                           (n ≥ 1)

topdecl
 := type simpletype = type
  | data [context =>] simpletype [= constrs]    [deriving]
  | newtype [context =>] simpletype = newconstr [deriving]
  | class [scontext =>] tycls tyvar     [where cdecls]
  | instance [scontext =>] qtycls inst  [where idecls]
  | default (type1 , … , typen)                                (n ≥ 0)
  | foreign fdecl
  | decl

decls := { decl1 ; … ; decln }                                 (n ≥ 0)

decl
 := gendecl
  | (funlhs | pat) rhs

cdecls := { cdecl1 ; … ; cdecln }                              (n ≥ 0)

cdecl
 := gendecl
  | (funlhs | var) rhs

idecls := { idecl1 ; … ; idecln }                              (n ≥ 0)

idecl
 := (funlhs | var) rhs
  | (empty)

gendecl
 := vars :: [context =>] type                       -- (type signature)
  | fixity [integer] ops                            -- (fixity declaration)
  |                                                 -- (empty declaration)

ops  := op1 , … , opn                                          (n ≥ 1)

vars := var1 , … , varn                                        (n ≥ 1)

fixity
 := infixl
  | infixr
  | infix
```

The declarations in the *syntactic category* `topdecls` are only allowed at the top level of a Haskell module, whereas `decls` may be used either at the top level or in nested scopes (i.e. within `let` or `where` constructs).

For exposition, we divide the declarations into 3 groups:

1. *User-defined datatypes* consisting of
  - `type` declarations
  - `newtype` declarations
  - `data` declarations

2. *Type classes and overloading* consisting of
  - `class` declarations
  - `instance` declarations
  - `default` declarations

3. *Nested declarations* consisting of
  - value bindings
  - type signatures
  - fixity declarations


Datatypes:
- primitive (hard-wired)
- builtin (normal types in std)

Haskell has several *primitive datatypes* that are hard-wired (such as integers and floating-point numbers), but most *builtin datatypes* are defined in normal Haskell code. The builtin datatypes are described in detail in Section 6.1.
