# Output and Printout

## Output in GHCi session

By default, GHCi prints the result of expressions typed at the prompt using `print`, which converts a value into string using `show`:

    `System.IO.print :: Show a => a -> IO ()`

This is not ideal because the Unicode text is rendered escaped. However, the new *-interactive-print* flag allows you to specify any function of type    
`C a => a -> IO ()` for some constraint C, as the function for printing evaluated expressions in GHCi. That function can reside in any loaded module or any registered package, but only when it resides in a registered package will it survive these commands: :cd, :add, :load, :reload, :set.


### Excessive Quotes

GHCi prints the result of expressions typed at the prompt using `print`, which converts a value into string using `show`. The `print` is always used, whether explicitly or implicitly.

> show [1..5]       -- "[1,2,3,4,5]"

> print [1..5]      --  [1,2,3,4,5]

> [1..5]            --  [1,2,3,4,5]


## sprint

The `sprint` function prints the current state of evaluation of an expression. It is used as an info tool that tries to show the properties and effects of lazy evaluation.
