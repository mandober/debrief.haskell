# Comonad identities

```hs
extend extract      == id                       -- (1)
extract . extend f  == f                        -- (2)
extend f . extend g == extend (f . extend g)    -- (3)

f =>= extract   == f                  -- LEFT_UNIT
extract =>= f   == f                  -- RIGHT_UNIT
(f =>= g) =>= h == f =>= (g =>= h)    -- ASSOC

f =<= g = f . extend g = flip (=>=)
f =>= g = g . extend f = flip (=<=)

extract . duplicate      == id
fmap extract . duplicate == id
duplicate . duplicate    == fmap duplicate . duplicate

extend f  == fmap f . duplicate
duplicate == extend id
fmap f    == extend (f . extract)

extract . fmap f == f . extract
fmap (fmap f) . duplicate == duplicate . fmap f
```



`ComonadApply` is to `Comonad` as `Applicative` is to `Monad`.

The laws:

```hs
(.) <$> u <@> v <@> w == u <@> (v <@> w)
    extract (p <@> q) == extract p (extract q)
  duplicate (p <@> q) == (<@>) <$> duplicate p <@> duplicate q


-- If the type is both a `ComonadApply` and `Applicative` we further require:
(<*>) == (<@>)

-- Finally, if you choose to define (`<@`) and (`@>`), the results of your definitions should match the following laws:
a @> b == const id <$> a <@> b
a <@ b == const <$> a <@> b


fmap f == liftW f == extend (f . extract)
liftW f == extend (f . extract)

(<@>) == ap
(<@@>) == liftW2 (flip id)

liftW2 f a b == f <$> a <@> b
liftW3 f a b c == f <$> a <@> b <@> c





-- Comonadic fixed point à la David Menendez
wfix :: Comonad w => w (w a -> a) -> a
wfix w = extract w (extend wfix w)

-- Comonadic fixed point à la Dominic Orchard
cfix :: Comonad w => (w a -> a) -> w a
cfix f = fix (extend f)

-- Comonadic fixed point à la Kenneth Foner:
-- This is the `evaluate` function from his talk:
-- https://www.youtube.com/watch?v=F7F-BzOB670 "Getting a Quick Fix on Comonads
kfix :: ComonadApply w => w (w a -> a) -> w a
kfix w = fix $ \u -> w <@> duplicate u
```
