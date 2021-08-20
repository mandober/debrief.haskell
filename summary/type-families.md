# Indexed Type Families

* `TypeFamilies` implies: ExplicitNamespaces,`KindSignatures`, MonoLocalBinds

* **Indexed Type Families (ITF)** is the most general name that includes all the different variants of the type families.

* Indexed Type Families provide *type-indexed datatypes* and *named type functions*.

* Type-indexed datatypes are types indexed with another type, called the *index*.

* Type Family : Type Class = overloaded data : overloaded functions

* Type Families have 3 dimensions:
  -   `type` vs `data`
  -     open vs closed
  - toplevel vs associted

* Enumaration of all indexed type families:
  - Associated open `type` family    (ex associated type synonym)
  - Toplevel   open `type` family
  - Toplevel closed `type` family
  + Toplevel   open `data` family
  + Associated open `data` family    (ex associated data type)

* Type families subsume these type class concepts:
  - associated data types
  - associated type synonyms


## Summary

* **Indexed Type Families (ITF)** is the most general name that includes all the different variants of the type families.

ITF attributes (dimensions):
-   `type` vs `data`
-     open vs closed
- toplevel vs associted


ITF classification:
- `type` families
  - open
    - toplevel
    - associated
  - closed
    - toplevel
- `data` families
  - open
    - toplevel
    - associated

ITF enumaration:
- Associated open `type` family
- Toplevel   open `type` family
- Toplevel closed `type` family
- Toplevel   open `data` family
- Associated open `data` family


Indexed type families | toplevel | associated
----------------------|----------|------------
Closed type family    |    ✅    |     ❌
Open type family      |    ✅    |     ✅
Open data family      |    ✅    |     ✅


ITF varaints:
- 5 variants total
- 1 closed, 4 open
- 3 toplevel, 2 associated
- 3 type families, 2 data families
- there are no closed data families
- there is no closed Associated Type Family
