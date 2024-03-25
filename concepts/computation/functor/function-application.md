# Function application

## On bare arguments

Straightforward function application means appling a function to naked (bare) arguments.

In Haskell, function application is denoted by juxtaposition `f x`, although there is also the operator `$` that more explicitly represents function application, `f x = f $ x`. The previous equation more-less stands; more precisely, the difference is that function application expressed by juxtaposition associates to the left (as is normal), i.e. `a b c = (a b) c`, while function application expressed as the dollar (`$`) associates to the rigth, i.e. `a (b c) = a $ b c = a $ (b c) = a $ b $ c`.

```hs
a = \ 'b' -> 'a'
b = \ 'c' -> 'b'
c = 'c'

_ = b c -- 'b'
_ = b $ c -- 'b'
_ = a $ b c -- 'a'
_ = a $ b $ c -- 'a'

_ = ((a (b c)) == (a $ b c)) == ((a $ (b c)) == (a $ b $ c))
```

This makes the `$` particularly useful as a limited parenthesis supression mechanism, but the `$` is also useful as a "reified" symbol for function application (mostly in sections, which are the result of eta-reductions, which means the same is expressable with eta-expanded function without the `$`).

```hs
-- using `$` instead of (some) parenthesis:
_ = f ((x - 1) + y) = f $ (x - 1) + y

-- using `$` to express function application:
_ = fmap (    $    7) [(* 3), (+ 5)]      -- [21,12]
_ = fmap (\ f -> f 7) [(* 3), (+ 5)]      -- [21,12]
```

## On boxed arguments

This is where functors come in. They "lift" a plain function, `a -> b`, propping it into a context, so it can be applied to the boxed args.

On the simplest boxes is `[a]` or `Identity a` that just holds some value of type `a`. This is sufficient to prohibit straightforward function application to the content, so this won't work `succ [5] = ✘`. The function `succ` needs to be lifted, i.e. levelling it with the content makes function application possible, `fmap succ [5] = [6]`. But the context (box) is left alone - the structure is preserved.

```hs
(<$>) = fmap

_ = succ   5   --  6
_ = succ $ 5   --  6

_ = succ     [5]  --  ✘
_ = succ <$> [5]  -- [6]

-- compare
f  $   a
f <$> [a]
```
