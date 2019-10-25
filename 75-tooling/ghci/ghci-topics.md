# GHCi topics

## Loading a module

Reverts the env to the last loaded module (Prelude)
> :m
To keep the already loaded modules and load a new one use
> :m + Data.List



## Browse exports of a module
```
Prelude> :browse Data.Bool
Data.Bool.bool :: a -> a -> Bool -> a
(&&) :: Bool -> Bool -> Bool
data Bool = False | True
not :: Bool -> Bool
otherwise :: Bool
(||) :: Bool -> Bool -> Bool
```



## Prompt
* :set prompt <prompt>   Set the prompt used in GHCi
* :set prompt2 <prompt>  Set the continuation prompt used in GHCi
* :show prompt           Show value of prompt setting

```
Prelude> :show prompt
"%s> "
```

After loading a source file, the prompt changes. To return to the `Prelude>` prompt, use `:module` cmd. This will unload the file from GHCi, so the code in that file will no longer be in scope in the REPL.
