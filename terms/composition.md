# Composition

Composition is frequently the right answer to most deliberate questions Haskell-related. "But, but, what is... the essence of this?", he said sami-excitedely, assured the answer was materializing in the middle, in from of everyone to see and shout it extatically in unison. But the silence lingered making him relapse back into questioning his teaching skills. "Composition?", semi-uttered one of the students who remembered the main advice from this article but refused to give it any credibility. The next thing that happend transpired quite naturally: the ceiling fan broke due to the number of stuck dicks.

> Take home advice: composition and S, K, B combinators.

Composition of compatible (but otherwise normal) functions is denoted by the dot operator. Composition doesn't do shit. It's entire purpose is to allow Haskell senseis to continue loathing data. Namely, it permits expressing the way two functions relate while actively ignoring the data involved. So `g . f` instead of unbearable `g (f x)` or semi-unbearable `g $ f x` decay. The dot composition has no sense of direction, at least considering the chosen symbol itself, but it is defined in a way that respect mathematical direction of right to left comosing, and reads `g . f` as "g after f" (although math tends to mention the data as well, `g . f [x]`).

However, more general sorts of composition do pull their weight. The next most popular composition is that of monadic functions done with the Kleisli arrow:
`g >=> f`. This one does quite a lot, having to compose functions that look like

```hs
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) = g . f

f :: a -> b
     g :: b -> c
h :: a   ->    c
h = g . f


(>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)
(<=<) :: (b -> m c) -> (a -> m b) -> (a -> m c)
(.)   :: (b ->   c) -> (a ->   b) -> (a ->   c)
```
