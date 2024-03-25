# Point-free rules

## Refs

* pointfree package
https://github.com/bmillwood/pointfree
https://github.com/bmillwood/pointfree/blob/main/Plugin/Pl/Rules.hs

* pointfree and pointful packages @Hackage
https://hackage.haskell.org/package/pointful
https://hackage.haskell.org/package/pointfree

* Pointfree at Haskell Wiki
https://wiki.haskell.org/Pointfree

* `Pointless Haskell` is library for point-free programming with recursion patterns defined as hylomorphisms.
https://webarchive.di.uminho.pt/wiki.di.uminho.pt/twiki/bin/view/Personal/Alcino/PointlessHaskell.html

* `Point-free Programming with Hylomorphisms`, Alcino Cunha



## Combinators


Combinators
- (f . g) x               <=> f (g x)
- flip id x . f             = flip f x
- s  f g x                  = f x (g x)
- s' g f x                  = f (g x) x    = s (flip f) g x

Isomorphisms
- flip (flip x) = x
- not (not x)   = x
- id . f        = f
- f . id        = f

Identity
- ($)                 -->> id
- concatMap           -->> (=<<)
- concat              -->> join
- liftM               -->> fmap
- map                 -->> fmap

Reduction
- id x         -->> x
- fst (x, y)   -->> x
- snd (x, y)   -->> y
- head (x:xs)  -->> x
- tail (x:xs)  -->> xs
- const x y    -->> x
- fmap id      -->> id
- map id       -->> id

- uncurry (,)         -->> id
- uncurry f (x, y)    -->> f x y
- uncurry f . s (,) g -->> s f g

- curry fst           -->> const
- curry snd           -->> const id

- flip f x y          -->> f y x
- flip (=<<)          -->> (>>=)

- (f . g) . h         -->> f . (g . h)

- map  f . map g      -->> map  (f . g)
- fmap f . fmap g     -->> fmap (f . g)
- subtract            -->> flip (-)

- (>>=)                    --> flip (=<<)
- (.) id                   --> id
- (++) [x]                 --> (:) x
- (=<<) return             --> id
- (=<<) f (return x)       --> f x
- (=<<) ((=<<) f . g)      --> (=<<) f . (=<<) g

- flip (f . g)             --> flip (.) g . flip f
- flip (.) f . flip id     --> flip f
- flip (.) f . flip flip   --> flip (flip . f)
- flip (flip (flip . f) g) --> flip (flip . flip f) g
- flip (.) id              --> id
- (.) . flip id            --> flip flip

- s const x y      --> y
- s (const . f) g  --> f
- s (const f)      --> (.) f
- s (f . fst) snd  --> uncurry f
- fst (join (,) x) --> x
- snd (join (,) x) --> x


- uncurry f (x,y)   --> f x y
- uncurry (curry f) --> f
- curry (uncurry f) --> f

- const id . f      --> const id
- const x  . f      --> const x
- const . f         --> flip (const f)
- flip const x      --> id
- fix (const f)     --> f

- f (fix f)         --> fix x
- fix f             --> f (fix x)
- fix f             --> f (f (fix x))

- not (x == y)      --> x /= y
- not (x /= y)      --> x == y

- 0 + x       --> x
- 0 * x       --> 0
- 1 * x       --> x
- x - x       --> 0
- x - y + y   --> x
- x + y - y   --> x
- x + (y - z) --> x + y - z
- x - (y + z) --> x - y - z
- x - (y - z) --> x + y - z


- join (fmap f x) --> f =<< x
- (=<<) id        --> join
- join            --> (=<<) id
- join (return x) --> x

- return . f =<< m          --> fmap f m

- (x >>=) . (return .) . f  --> flip (fmap . f) x
- (>>=) (return f)          --> flip id f

- liftM2 f x                --> ap (f `fmap` x)
- liftM2 f (return x)       --> fmap (f x)

- f `fmap` return x         --> return (f x)
- (=<<) . flip (fmap . f)   --> flip liftM2 f

- (.)               --> fmap

- zipWith (,)       --> zip (,)
- map f (zip xs ys) --> zipWith (curry f) xs ys

- all f       --> and . map f
- and . map f --> all f

- any f       --> or . map f
- or . map f  --> any f

- return f `ap` x           --> fmap f x
- ap (f `fmap` x)           --> liftM2 f x
- f `ap` x                  --> (`fmap` x) =<< f
- (`fmap` x) =<< f          --> f `ap` x
- (x >>=) . flip (fmap . f) --> liftM2 f x



- (f =<< m) x    --> f (m x) x
- fmap f g x     --> f (g x)

- return x y     --> x

- liftM2 f g h x --> g x `h` h x
- ap f id        --> join f
- (=<<) const q  --> flip (>>) q
- p >> q         --> const q =<< p

experimental support for Control.Arrow:
- uncurry ((. g) . (,) . f) --> f *** g
- uncurry ((,) . f)         --> first f
- uncurry ((. g) . (,))     --> second g
- uncurry (const f)         --> f . snd
- uncurry const             --> fst
- uncurry (const . f)       --> f . fst

- [x]              --> return x
- length []        --> 0
- length (x:xs)    --> 1 + length xs

map/fmap elimination:
- map f (x:xs)     --> f x: map f xs
- fmap f (x:xs)    --> f x: Fmap f xs
- map f []         --> []
- fmap f []        --> []

foldr elimination:
- foldr f z (x:xs) --> f x (foldr f z xs)
- foldr f z []     --> z

foldl elimination:
- sum xs           --> foldl (+) 0 xs
- product xs       --> foldl (*) 1 xs
- foldl1 f (x:xs)  --> foldl f x xs
- foldl f z (x:xs) --> foldl f (f z x) xs
- foldl f z []     --> z

special rule:
- foldl f z [x]   --> f z x
- (:) x           --> (++) [x]
- (:) x . (++) ys --> (++) (x:ys)
