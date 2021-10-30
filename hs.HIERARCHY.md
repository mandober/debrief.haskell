# Haskell: HIERARCHY

## TOC
- [Program Structure](#program-structure)
- [Namespaces](#namespaces)
- [Elementary concepts](#elementary-concepts)
- [Lexical structure](#lexical-structure)
- [Type-level concepts](#type-level-concepts)
- [Algebraic data types](#algebraic-data-types)
- [Type-level programming](#type-level-programming)

## Program Structure
  - abstract syntactic structure
  - semantic structure
  - syntactic structure
    - topmost    level: modules
    - module     level: declarations
    - expression level: values at static types
      - expression evaluates to a value and has a static type
    - bottom     level: lexical structure, concrete representation
  - syntactic levels
    - term level
    - type level
    - kind level

## Namespaces
  1. value namespace
    - variables
    - data constructors
  2. type namespace
    - type variables
    - type constructors
    - type classes
    - type families
  3. module namespace
    - module names
  - package names (to qualify modules)

## Elementary concepts
  - term
  - value
  - expression
  - type, datatype
  - namespace
  - syntax
  - semantics

## Lexical structure
  - notational conventions
  - lexical program structure
  - comments
  - layout
  - identifiers
  - operators
  - literals
    - numeric literals
    - character literals
    - string literals

## Type-level concepts
  - type, data type
  - abstract data type
  - user-created data types
  - algebraic data types

## Algebraic data types
  - Void, (0)
  - unit, (1)
  - sum types, (+)
  - product types, (⨯)
  - exponentail types (->)
  - inductive types, (μ)

## Type-level programming
  - type-level functions
  - unsaturated type ctors
  - type families
  - ADTs
  - GADTs
  - classes
    - inductive classes
    - multivariable classes
