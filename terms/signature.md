# Signatures










## ScopedTypeVariables

The *ScopedTypeVariables* extension makes the type params scoped and hence available in the current (function, type, instance) definition. This is usually useful in functions that have nested helper functions, so that a helper function is able to reference a TP declared in the signature of the enclosing function.

Two conditions needs to be met in order for TPs to be scoped: enabling the language pragma, and explicitly introducing the TP with the `forall`.

Otherwise, even though a TP has the same name as the one introduced earlier, it is considered as a new TP so a fresh type param will be spawned.



## Signature forms

Random sampling of syntactical forms of signatures, especially with consideration to kinds, `forall` quantifier.
