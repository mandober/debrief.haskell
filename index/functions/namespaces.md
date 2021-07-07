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
