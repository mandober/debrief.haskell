# Namespace

- type and data ctors are in different namespace



6 kinds of names
* entities that denote values
  - variable (lowercase)
  - data constructor
* type system entities
  - type variable (lowercase)
  - type constructor
  - type class
* modules
  - module


- same name cannot be used for class and type ctor (in same scope and module)
- within a single scope, `Int` may simultaneously be the name of a module, class, type ctor, data ctor


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




## Comments

The sequence of dashes must not form part of a legal lexeme. For example, “-->” or “|--” do not begin a comment, because both of these are legal lexemes; however “--foo” does start a comment.

In an ordinary comment, the character sequences “{-” and “-}” have no special significance, and, in a nested comment, a sequence of dashes has no special significance.


## Identifiers and Operators

- *identifier* consists of a letter followed by 0 or more letters, digits, underscores and single quotes.
- Identifiers are lexically distinguished into two namespaces:
  1. those that begin with a lowercase letter (variable identifiers)
  2. those that begin with an upper-case letter (constructor identifiers)
- Identifiers are case sensitive

- underscore is treated as a lowercase letter, and can occur wherever a lowercase letter can
- underscore by itself is a reserved identifier used as *wild card* in patterns
- Compilers that offer warnings for unused identifiers are encouraged to suppress such warnings for identifiers beginning with underscore. This allows programmers to use `_foo` for a parameter they expect to be unused.


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
