# Namespace

Haskell's namespace is split into 3 categories, each with its own subcategories, totaling to 6 different kinds of names (identifiers). 

1. Names of values are in the value namespace, *ns-values*
  - variables           (lowercase)
  - data constructors   (UPPERCASE)
2. entities related to type-system are in `types-namespace`, *ns-types*
  - type variables      (lowercase)
  - type constructors   (UPPERCASE)
  - type classes        (UPPERCASE)
3. module names are in `module-namespace`, *ns-modules*
  - modules             (UPPERCASE)

Within the same scope, the same name may be used to refer to one thing from each category.

* ns-values  : vars, DataCtors
* ns-types   : typeParams, TypeCtors, TypeClasses
* ns-modules : Modules (packages?)

So you can use the same name but only for one entity from each of the 3 categories, e.g. in the same scope, `Int` may simultaneously be the name of a data ctor (ns-values), type class (ns-types) and module (ns-modules).

That is, there is nothing to choose from *ns-modules* as only the names of modules themselves belong to this category. Next, concerning the *ns-values*, there is not much of a choice either since a module's name had to start with a capital letter. Thus, you can only have the same-named data ctor. There is osme choice only in the *ns-types* - you can choose between having the same-named type ctor or a type class. In practice, people often exploit this to have the eponymous type and one of the data ctors (especially with unary types).


## Names of language entities

There are 6 kinds of names in Haskell
* names that denote *values* (value-names)
  1. variables
  2. (data) constructors
* names that refer to *type system* entities (type-system-names)
  3. type variables
  4. type constructors
  5. type classes
* names that refer to *modules* (module-names)
  6. module names

There are 2 constraints on naming:
* Names for variables and type variables are identifiers beginning with lowercase letters or underscore; the other 4 kinds of names are identifiers beginning with uppercase letters.
* An identifier must not be used as the name of a type constructor and a class in the same scope.
* These are the only constraints; for example, `Int` may simultaneously be the name of a *module*, *class*, and *constructor* within a single scope.


**Packages** are referred to by name, more strictly, a *package id* is a particular version of the package made of package name and version number. A package name may only contain alphanumerics and dashes, e.g. `base-4.14.0.0`.



```hs
> data Sheep = MkSheep { name :: String, id :: Int }

> :t Sheep
Sheep :: String -> Int -> Ghci1.Sheep
> :t MkSheep
MkSheep :: String -> Int -> Sheep
> :t name
name :: Sheep -> String
> :t id
id :: Sheep -> Int
```


### Operators

Operator symbols are formed from one or more symbol characters, as defined above (varSym, conSym, reservedOp).

Operator symbols are lexically distinguished into 2 namespaces:
1. operator symbol starting with a colon is a constructor
2. operator symbol starting with any other character is an ordinary identifier

- The colon by itself is reserved solely for use as the list constructor
- This makes it uniform with other parts of list syntax, such as `[]`, `[a,b]`
- Other than the special syntax for prefix negation, all operators are infix
- each infix op can be used in a section for partially applied operators
- All of standard infix ops are just predefined symbols and may be rebound


## 2.5 Numeric Literals

There are 2 distinct kinds of numeric literals:
* Integer literals may be given in
  - decimal (default)
  - octal when prefixed by `0o` or `0O`
  - hexadecimal when prefixed by `0x` or `0X`
* Floating literals
  - always decimal
  - must contain digits both before and after the decimal point
  - this ensures that a decimal point cannot be mistaken for dot


## 2.6 Character and String Literals

escape  = \ ( charesc | ascii | decimal | o octal | x hexadecimal )
charesc = `a b f n r t v \ " ' &`
cntrl   = ascLarge | `@ [ \ ] | ^ _`
ascii   = ^cntrl
| NUL | SOH | STX | ETX | EOT | ENQ | ACK | BEL     
| BS  | HT  | LF  | VT  | FF  | CR  | SO  | SI      
| DLE | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB     
| CAN | EM  | SUB | ESC | FS  | GS  | RS  | US    
| SP     
| DEL     
(0-31, 32=SP, 127=DEL)

- Character literals are written between single quotes
- strings   literals are written between double quotes
- Escape codes may be used in characters and strings to represent special chars
- single quote may be used in a string, but must be escaped in a char
- double quote may be used in a char, but must be escaped in a string.
- `\` must always be escaped
-  charesc also includes portable representations for the characters:
  - alert, `\a`
  - backspace, `\b`
  - form feed, `\f`
  - new line, `\n`
  - carriage return, `\r`
  - horizontal tab, `\t`
  - vertical tab, `\v`




# List of namespaces

The list of items that can be named.

Basically, proper (named) functions must be given a name, and the name is the allowed identifier, which in Haskell can be either:
- alphanumeric (classic name as in most PLs)
  - identifier: _?[a-z_][a-zA-Z0-9_]+'*
  - except that Haskell also allows
- symbolic name


Haskell's namespace is split into 3 categories, each with its own subcategories, totaling to 6 different kinds of names (identifiers):
1. Names of values are in the value namespace, *ns-values*
  - variable
  - data ctor
2. entities related to type-system are in types-namespace, *ns-types*
  - type var
  - type ctor
  - type class
3. module names are in module-namespace, *ns-modules*
  - module
* other names
  - package
  - import...as
  - pattern
