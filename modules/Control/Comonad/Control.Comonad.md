# Control.Comonad

The package `comonad` https://hackage.haskell.org/package/comonad-5.0.8 provides several modules that are arranged in the existing hierarchy under the `Control` and `Data` namespace.

- Control
  - Comonad                           Control.Comonad
    - Env                               Control.Comonad.Env
      - Class                             Control.Comonad.Env.Class
    - Hoist
      - Class                           Control.Comonad.Hoist.Class
    - Identity                          Control.Comonad.Identity
    - Store                             Control.Comonad.Store
      - Class                             Control.Comonad.Store.Class
    - Traced                            Control.Comonad.Traced
      - Class                             Control.Comonad.Traced.Class
    - Trans
      - Class                           Control.Comonad.Trans.Class
      - Env                             Control.Comonad.Trans.Env
      - Identity                        Control.Comonad.Trans.Identity
      - Store                           Control.Comonad.Trans.Store
      - Traced                          Control.Comonad.Trans.Traced
- Data
  - Functor
    - Composition                       Data.Functor.Composition
