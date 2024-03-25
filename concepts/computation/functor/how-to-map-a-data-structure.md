# How to map a data structure

1. *updating* (presumes a mapping function; an updating function)
   Applying a function on each element of the structure (`fmap`).
2. *replacing*
  - using a provided value (diff for each mapping)
    Replacing each element with the provided value (`<$`, `$>`).
  - using a fixed constant value, like (), (same for each mapping)
    Replacing each element with the same constant value, e.g. (), (`void`).

Sometime we perform the actual *mapping*, and sometimes we just perform *replacement*.

**Mapping** is like *updating* a set of values - new values are computed by applying a function to each existing value; but in order to compute a new value, we first need to retrieve the current one - the getter precludes the update. **Replacing**, on the other hand, is like blindly *overwriting* existing values without caring about existing values. However, we usually only talk about mapping - it subsumes replacing.

- `fmap` can be thought of as *mapping* or *substitution*.
- Monads provide substitution (`fmap`) and renormalization (`join`).
- `>>=` can also be interpreted as *substitution* with name binding (?). 
- >m >>= f = join (fmap f m)
