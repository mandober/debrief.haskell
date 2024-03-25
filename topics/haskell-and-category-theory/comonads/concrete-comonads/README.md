# Concrete comonads

- Env (co-Reader), `Env e a = Env (a, a)`
- Store (co-State), `Store s a = Store ((s -> a) s)`
- Traced (co-Writer), `Traced w a = Monoid m => m -> a`


* Chris Penner - Comonads By Example 3/4 - YouTube
https://www.youtube.com/watch?v=FtaT73bpEGs&list=PLcAu_kKy-krxDD1WwRX_9rc0knAFK3nHs&index=11

* ACT 2020 Tutorial: Monads and comonads (Paolo Perrone) - YouTube
https://www.youtube.com/watch?v=ryMkvAOJk20&t=262s

* The Comonad.Reader » Monads from Comonads
http://comonad.com/reader/2011/monads-from-comonads/

* The Comonad.Reader » Monad Transformers from Comonads
http://comonad.com/reader/2011/monad-transformers-from-comonads/

* The Comonad.Reader » More on Comonads as Monad Transformers
http://comonad.com/reader/2011/more-on-comonads-as-monad-transformers/
