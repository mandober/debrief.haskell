# Haskell and category theory

## Possible categories in Haskell

Category theory deals with an infinite number of categories, but Haskell is not that rich. A category that is usually identified is one of Haskell types and functions, which even has the more-less standardized name `Hask`. Funny enough, under the (strict) definition of a category, `Hask` fails miserably due to the bottom-infested types - all types contain the bottom value which is needed in order to represent diverging computation. However, the standard approach dictates that it is ok to ignore this issue and proceed with this (so-called) "fast and loose reasoning"[^1].





is not even a category because all types are infested with the bottom value

despite not even being a category under the strict  dur to bottom


There is only a single category that has a somewhat the category, of .

This is not to say that there are no other categories. After all, it is enough to find a single category for there to be an infinite number of subcategories.


[^1] "Functional programmers often reason about programs as if they were written in a total language, expecting the results to carry over to non-total (partial) languages. We justify such reasoning" was the introductory statement of the 2006's paper `Fast and Loose Reasoning is Morally Correct` by Nils Anders Danielsson, John Hughes, Patrik Jansson, Jeremy Gibbons.
