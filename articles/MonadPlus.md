---
downloaded:       2022-01-03
page-url:         https://wiki.haskell.org/MonadPlus
page-title:       MonadPlus - HaskellWiki
article-title:    MonadPlus - HaskellWiki
---
# MonadPlus - HaskellWiki

The MonadPlus class is defined like this:
MonadPlus class ([base][1])

import [Control.Monad][2]

The **MonadPlus** class is defined like this:

class (Monad m) \=> MonadPlus m where
   mzero :: m a
   mplus :: m a \-> m a \-> m a

The precise set of rules that MonadPlus should obey is not agreed upon.

-   **Monoid** — `mplus` and `mzero` form a monoid:

mplus mzero a \= a
mplus a mzero \= a
mplus (mplus a b) c \= mplus a (mplus b c)

-   **Left Zero** — `mzero` is a left zero for `(>>=)`:

-   **Left Distribution**:

mplus a b \>>= k \= mplus (a \>>= k) (b \>>= k)

-   **Left Catch** — this is rarely advocated, but Maybe and IO satisfy this as an alternative to **Left Distribution**.

mplus (return a) b \= return a

### Which satisfies what?

-   `[]` satisfies **Monoid**, **Left Zero**, and **Left Distribution**.

-   `Maybe`, `IO` and `STM` satisfy **Monoid**, **Left Zero**, and **Left Catch**.

## Which rules?

[Martin & Gibbons][3] choose **Monoid**, **Left Zero**, and **Left Distribution**. This makes `[]` a `MonadPlus`, but not `Maybe` or `IO`.

## What should be done?

It is proposed that the class be separated into `MonadZero`, `MonadPlus`, `MonadOr`. See [MonadPlus reform proposal][4].

[1]: https://wiki.haskell.org/Base_package "Base package"
[2]: https://hackage.haskell.org/package/base/docs/Control-Monad.html#t%3AMonadPlus
[3]: http://web.comlab.ox.ac.uk/oucl/work/jeremy.gibbons/publications/tactics.pdf
[4]: https://wiki.haskell.org/MonadPlus_reform_proposal "MonadPlus reform proposal"
