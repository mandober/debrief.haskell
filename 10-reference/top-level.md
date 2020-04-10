# Top-level


Top-level declarations
- `data`
- `type`
- `newtype`
- `class`
- `instance`
- `deriving`
- `foreign`



* Haskell module is a collection of related functions, types and typeclasses.
* Haskell program consists of a set of modules.
* Modules serve the purpose of controlling the namespaces
* Modules are declarations that contain other declarations, including
    - type declarations
    - classes, class instances
    - function signatures and their definitions
* namespace is a set of objects (functions, classes)
  organized and contained so that they may be referenced by name, 
  without interference from other objects with the same name.
* namespace of modules is flat: declarations may appear in any order
  save for module header and import declarations)
