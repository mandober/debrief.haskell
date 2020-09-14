# Lifted types

**Lifted types** include bottom (⊥) value (`undefined :: ⊥`). Lifted types are all boxed (pointer to the heap data).

**Unlifted types** are the most fundamental machine primitives (e.g. integer and float) and they don't have bottom value. Their kind is `#`. They are unboxed values that usually live on the stack, although some unlifted types are boxed (Array).
