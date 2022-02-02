---
downloaded:       2022-01-01
page-url:         https://wiki.haskell.org/ListT_done_right
page-title:       ListT done right - HaskellWiki
article-title:    ListT done right - HaskellWiki
---
# ListT done right - HaskellWiki

The Haskell hierarchical libraries implement a ListT monad transformer. There are, however, some problems with that implementation.
## Introduction

The Haskell hierarchical libraries implement a ListT monad transformer. There are, however, some problems with that implementation.

-   `ListT` imposes unnecessary strictness.
-   `ListT` isn't really a monad transformer, ie. `ListT m` isn't always a monad for a monad `m`.

See the [#Examples][1] below for demonstrations of these problems.

## Implementation

The following implementation tries to provide a replacement for the ListT transformer using the following technique. Instead of associating a monadic side effect with a list of values (`m [a]`), it lets each element of the list have its own side effects, which only get \`excecuted' if this element of the list is really inspected.

There is also a [ListT done right alternative][2], the [Pipes][3] package, which provides its own version of ListT and there is the ["list-t"][4] package.

import Data.Maybe
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.Cont

\-- The monadic list type
data MList' m a \= MNil | a \`MCons\` MList m a
type MList m a  \= m (MList' m a)

\-- This can be directly used as a monad transformer
newtype ListT m a \= ListT { runListT :: MList m a }

\-- A "lazy" run function, which only calculates the first solution.
runListT' :: Functor m \=> ListT m a \-> m (Maybe (a, ListT m a))
runListT' (ListT m) \= fmap g m where
  g MNil \= Nothing
  g (x \`MCons\` xs) \= Just (x, ListT xs)

\-- In ListT from Control.Monad this one is the data constructor ListT, so sadly, this code can't be a drop-in replacement.
liftList :: Monad m \=> \[a\] \-> ListT m a
liftList \[\] \= ListT $ return MNil
liftList (x:xs) \= ListT . return $ x \`MCons\` (runListT $ liftList xs)

instance Functor m \=> Functor (ListT m) where
  fmap f (ListT m) \= ListT $ fmap (fmap f) m

instance Functor m \=> Functor (MList' m) where  
  fmap \_ MNil \= MNil
  fmap f (x \`MCons\` xs) \= f x \`MCons\` fmap (fmap f) xs

\-- Why on earth isn't Monad declared \`class Functor m => Monad m'?
\-- I assume that a monad is always a functor, so the contexts 
\-- get a little larger than actually necessary
instance (Functor m, Monad m) \=> Monad (ListT m) where
  return x \= ListT . return $ x \`MCons\` return MNil
  m \>>= f \= joinListT $ fmap f m

instance MonadTrans ListT where
  lift \= ListT . liftM (\`MCons\` return MNil)

instance (Functor m, Monad m) \=> MonadPlus (ListT m) where
  mzero \= liftList \[\]
  (ListT xs) \`mplus\` (ListT ys) \= ListT $ xs \`mAppend\` ys

\-- Implemenation of join
joinListT :: (Functor m, Monad m) \=> ListT m (ListT m a) \-> ListT m a
joinListT (ListT xss) \= ListT . joinMList $ fmap (fmap runListT) xss

joinMList :: (Functor m, Monad m) \=> MList m (MList m a) \-> MList m a
joinMList \= (\=<<) joinMList'

joinMList' :: (Functor m, Monad m) \=> MList' m (MList m a) \-> MList m a
joinMList' MNil \= return MNil
joinMList' (x \`MCons\` xs) \= x \`mAppend\` joinMList xs

mAppend :: (Functor m, Monad m) \=> MList m a \-> MList m a \-> MList m a
mAppend xs ys \= (\`mAppend'\` ys) \=<< xs

mAppend' :: (Functor m, Monad m) \=> MList' m a \-> MList m a \-> MList m a
mAppend' MNil           ys \= ys
mAppend' (x \`MCons\` xs) ys \= return $ x \`MCons\` mAppend xs ys

\-- These things typecheck, but I haven't made sure what they do is sensible.
\-- (callCC almost certainly has to be changed in the same way as throwError)
instance (MonadIO m, Functor m) \=> MonadIO (ListT m) where
  liftIO \= lift . liftIO

instance (MonadReader s m, Functor m) \=> MonadReader s (ListT m) where
  ask     \= lift ask
  local f \= ListT . local f . runListT

instance (MonadState s m, Functor m) \=> MonadState s (ListT m) where
  get \= lift get
  put \= lift . put

instance (MonadCont m, Functor m) \=> MonadCont (ListT m) where
  callCC f \= ListT $
    callCC $ \\c \->
      runListT . f $ \\a \-> 
        ListT . c $ a \`MCons\` return MNil

instance (MonadError e m, Functor m) \=> MonadError e (ListT m) where
  throwError       \= lift . throwError
{- This (perhaps more straightforward) implementation has the disadvantage
 that it only catches errors that occur at the first position of the 
 list.
 m \`catchError\` h = ListT $ runListT m \`catchError\` \\e -> runListT (h e)
\-}
  \-- This is better because errors are caught everywhere in the list.
  (m :: ListT m a) \`catchError\` h \= ListT . deepCatch . runListT $ m 
      where
    deepCatch :: MList m a \-> MList m a
    deepCatch ml \= fmap deepCatch' ml \`catchError\` \\e \-> runListT (h e)
    
    deepCatch' :: MList' m a \-> MList' m a
    deepCatch' MNil \= MNil 
    deepCatch' (x \`MCons\` xs) \= x \`MCons\` deepCatch xs

## Examples

Here are some examples that show why the old ListT is not right, and how to use the new ListT instead.

### Sum of squares

Here's a silly example how to use ListT. It checks if an `Int` `n` is a sum of two squares. Each inspected possibility is printed, and if the number is indeed a sum of squares, another message is printed. Note that with our ListT, runMyTest only evaluates the side effects needed to find the first representation of `n` as a sum of squares, which would be impossible with the ListT implementation of `Control.Monad.List.ListT`.

myTest :: Int \-> ListT IO (Int, Int)
myTest n \= do
  let squares \= liftList . takeWhile (<=n) $ map (^(2::Int)) \[0..\]
  x <- squares
  y <- squares
  lift $ print (x,y)
  guard $ x + y \== n
  lift $ putStrLn "Sum of squares."
  return (x,y)
  
runMyTest :: Int \-> IO (Int, Int)  
runMyTest \= fmap (fst . fromJust) . runListT' . myTest

A little example session (`runMyTest'` is implemented in exactly the same way as `runMyTest`, but uses `Control.Monad.List.ListT`):

\*Main> runMyTest 5
(0,0)
(0,1)
(0,4)
(1,0)
(1,1)
(1,4)
Sum of squares.
\*Main> runMyTest' 5
(0,0)
(0,1)
(0,4)
(1,0)
(1,1)
(1,4)
Sum of squares.
(4,0)
(4,1)
Sum of squares.
(4,4)

### Grouping effects

I didn't understand the statement "`ListT m` isn't always a monad", even after I understood why it is too strict. I found the answer in [Composing Monads][5]. It's in fact a direct consequence of the unnecessary strictness. `ListT m` is not associative (which is one of the monad laws), because grouping affects when side effects are run (which may in turn affect the answers). Consider

import Control.Monad.List
import Data.IORef

test1 :: ListT IO Int
test1 \= do
  r <- liftIO (newIORef 0)
  (next r \`mplus\` next r \>> next r \`mplus\` next r) \>> next r \`mplus\` next r

test2 :: ListT IO Int
test2 \= do
  r <- liftIO (newIORef 0)
  next r \`mplus\` next r \>> (next r \`mplus\` next r \>> next r \`mplus\` next r)

next :: IORef Int \-> ListT IO Int
next r \= liftIO $ do  x <- readIORef r
                      writeIORef r (x+1)
                      return x

Under Control.Monad.List.ListT, test1 returns the answers `[6,7,8,9,10,11,12,13]` while test2 returns the answers `[4,5,6,7,10,11,12,13]`. Under the above ListT (if all answers are forced), both return `[2,3,5,6,9,10,12,13]`.

[Andrew Pimlott][6]

### Order of printing

Here is another (simpler?) example showing why "`ListT m` isn't always a monad".

a,b,c :: ListT IO ()
\[a,b,c\] \= map (liftIO . putChar) \['a','b','c'\]

t1 :: ListT IO ()
t1 \= ((a \`mplus\` a) \>> b) \>> c

t2 :: ListT IO ()
t2 \= (a \`mplus\` a) \>> (b \>> c)

Under `Control.Monad.List.ListT`, running `runListT t1` prints "aabbcc", while `runListT t2` instead prints "aabcbc". Under the above ListT, they both print "abc" (if all answers were forced, they would print "abcabc").

[Roberto Zunino][7]

### Order of `ListT []`

This is a simple example that doesn't use `IO`, only pure `ListT []`.

v :: Int \-> ListT \[\] Int
v 0 \= ListT \[\[0, 1\]\]
v 1 \= ListT \[\[0\], \[1\]\]

main \= do
    print $ runListT $ ((v \>=> v) \>=> v) 0
    \-- = \[\[0,1,0,0,1\],\[0,1,1,0,1\],\[0,1,0,0\],\[0,1,0,1\],\[0,1,1,0\],\[0,1,1,1\]\]
    print $ runListT $ (v \>=> (v \>=> v)) 0
    \-- = \[\[0,1,0,0,1\],\[0,1,0,0\],\[0,1,0,1\],\[0,1,1,0,1\],\[0,1,1,0\],\[0,1,1,1\]\]

Clearly, `ListT []` fails to preserve the associativity monad law.

This example violates the requirement given in [the documentation][8] that the inner monad has to be commutative. However, all the preceding examples use `IO` which is neither commutative, so I suppose this example is valid at the end. Most likely, a proper implementation of `ListT` should not have such a requirement.

\--[PetrP][9] 19:15, 27 September 2012 (UTC)

## Relation to Nondet

NonDeterminism describes another monad transformer that can also be used to model nondeterminism. In fact, `ListT` and `NondetT` are quite similar with the following two functions translating between them

toListT :: (Monad m) \=> NondetT m a \-> ListT m a
toListT (NondetT fold) \= ListT $ fold ((return.) . MCons) (return MNil)

toNondetT :: (Monad m) \=> ListT m a \-> NondetT m a
toNondetT (ListT ml) \= NondetT (\\c n \-> fold c n ml) where
  fold :: Monad m \=> (a \-> m b \-> m b) \-> m b \-> MList m a \-> m b
  fold c n xs \= fold' c n \=<< xs

  fold' :: Monad m \=> (a \-> m b \-> m b) \-> m b \-> MList' m a \-> m b
  fold' \_ n MNil \= n
  fold' c n (x \`MCons\` xs) \= c x (fold c n xs)

`ListT` is smaller than `NondetT` in the sense that `toListT . toNondetT` is the identity (is it ok to call `ListT` \`retract'?). However, these functions don't define an isomorphism (check for example `NondetT (\_ n -> liftM2 const n n)`).

[Thomas Jaeger][10]

*I propose to replace every occurence of \`fmap\` in the above code with \`liftM\`, thereby moving \`class Functor\` and the complaint about it not being a superclass of \`Monad\` completely out of the picture. I'd simply do it, if there wasn't this feeling that I have overlooked something obvious. What is it? -- [Udo Stenzel][11]*

There's no particular reason why I used fmap, except that the page has the (unfortunate!) title "ListT Done Right", and having Functor superclass of Monad certainly is the right thing. But I agree, that mistake has long been done and I feel my half-hearted cure is worse than the disease. You can find an alternative, more concise definition of a ListT transformer based on even-style lists here: [ListT done right alternative][12]

[amb][13] has AmbT, which could be considered as 'ListT done right' (since Amb is identical to the list monad).

[1]: https://wiki.haskell.org/ListT_done_right#Examples
[2]: https://wiki.haskell.org/ListT_done_right_alternative "ListT done right alternative"
[3]: https://wiki.haskell.org/Pipes "Pipes"
[4]: http://hackage.haskell.org/package/list-t
[5]: http://web.cecs.pdx.edu/~mpj/pubs/composing.html
[6]: https://wiki.haskell.org/index.php?title=Andrew_Pimlott&action=edit&redlink=1 "Andrew Pimlott (page does not exist)"
[7]: https://wiki.haskell.org/index.php?title=Roberto_Zunino&action=edit&redlink=1 "Roberto Zunino (page does not exist)"
[8]: http://hackage.haskell.org/packages/archive/mtl/latest/doc/html/Control-Monad-List.html
[9]: https://wiki.haskell.org/index.php?title=User:Petr_Pudlak&action=edit&redlink=1 "User:Petr Pudlak (page does not exist)"
[10]: https://wiki.haskell.org/index.php?title=Thomas_Jaeger&action=edit&redlink=1 "Thomas Jaeger (page does not exist)"
[11]: https://wiki.haskell.org/index.php?title=Udo_Stenzel&action=edit&redlink=1 "Udo Stenzel (page does not exist)"
[12]: https://wiki.haskell.org/ListT_done_right_alternative "ListT done right alternative"
[13]: https://wiki.haskell.org/Amb "Amb"
