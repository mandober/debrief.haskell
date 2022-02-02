# Type family flavours

Indexed type families
- Type families
  - Closed type families
    - top-level
    - (no associated closed type families)
  - Open type families
    - top-level
    - associated with a class
- Data families
  - Open data families
    - top-level
    - associated
  - (no closed data families)


## Summary

**Indexed Type Families (ITF)** is the most general name that includes all the different variants of the type families.

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
Open   data family    |   ✔      |   ✔
Open   type family    |   ✔      |   ✔
Closed type family    |   ✔      |   ✘


ITF varaints:
- 5 variants total
- 1 closed, 4 open
- 3 toplevel, 2 associated
- 3 type families, 2 data families
- there are no closed data families
- there is no closed Associated Type Family
