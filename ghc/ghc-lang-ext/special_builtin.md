Special built-in functions {#special-ids}
==========================

GHC has a few built-in functions with special behaviour. In particular:

-   `GHC.Exts.inline`{.interpreted-text role="base-ref"} allows control
    over inlining on a per-call-site basis.
-   `GHC.Exts.lazy`{.interpreted-text role="base-ref"} restrains the
    strictness analyser.
-   `GHC.Exts.oneShot`{.interpreted-text role="base-ref"} gives a hint
    to the compiler about how often a function is being called.
