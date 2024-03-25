---
downloaded:       2022-01-01
page-url:         https://wiki.haskell.org/MonadFix
page-title:       MonadFix - HaskellWiki
article-title:    MonadFix - HaskellWiki
---
# MonadFix - HaskellWiki

The MonadFix typeclass provides the mfix method for value recursion. It can be used directly, or indirectly through the RecursiveDo extension. The GHC Users Guide has a section on RecursiveDo. It is useful for building cyclic data in monadic code.
  
The MonadFix typeclass provides the `mfix` method for value recursion. It can be used directly, or indirectly through the RecursiveDo extension. **The [GHC Users Guide][1] has [a section on RecursiveDo][2].** It is useful for building cyclic data in monadic code.

## What it is not and what it is

It is tempting to see “recursion” and guess it means performing actions recursively or repeatedly. **No**. It means recursion over values passed into and returned by actions; this is why it is called “value recursion”. An action may use a value to be returned by the same action, or even returned by another action several lines of code later. Some uses are: creating cyclic data structures; using monadic code to specify a graph or network.

## Examples

### Imperative cyclic linked lists

This example creates linked lists the imperative way: each node has a number and a pointer to the next node; a pointer is an `IORef`. (For simplicity, the number is immutable here, but this can be changed.) We can create a node that points back to itself. The message “node created” is printed only once, demonstrating that the action is not recursively run; rather, the pointer is recursively made/used.

with rec-syntax

with mfix

with mdo-syntax

{-# LANGUAGE RecursiveDo #-}
import Data.IORef
data Node \= Node Int (IORef Node)
mknode \= do
    rec p <- newIORef (Node 0 p)
    putStrLn "node created"
    return p
main \= do
  p <- mknode
  Node x q <- readIORef p
  print x
  Node y \_ <- readIORef q
  print y

import Control.Monad.Fix
import Data.IORef
data Node \= Node Int (IORef Node)
mknode \= mfix (\\p \-> do
    p' <- newIORef (Node 0 p)
    putStrLn "node created"
    return p')
main \= do
  p <- mknode
  Node x q <- readIORef p
  print x
  Node y \_ <- readIORef q
  print y

{-# LANGUAGE RecursiveDo #-}
import Data.IORef
data Node \= Node Int (IORef Node)
mknode \= mdo
  p <- newIORef (Node 0 p)
  putStrLn "node created"
  return p
main \= do
  p <- mknode
  Node x q <- readIORef p
  print x
  Node y \_ <- readIORef q
  print y

We can also create two nodes pointing to each other. A line of code can use an `IORef` obtained in a later line.

with rec-syntax

with mfix

with mdo-syntax

{-# LANGUAGE RecursiveDo #-}
import Data.IORef
data Node \= Node Int (IORef Node)
mk2nodes \= do
    rec p <- newIORef (Node 0 r)
        r <- newIORef (Node 1 p)
    putStrLn "nodes created"
    return p

main \= do
  p <- mk2nodes
  Node x q <- readIORef p
  print x
  Node y \_ <- readIORef q
  print y

import Control.Monad.Fix
import Data.IORef
data Node \= Node Int (IORef Node)
mk2nodes \= mfix (\\ ~(p,r) \-> do
    p' <- newIORef (Node 0 r)
    r' <- newIORef (Node 1 p')
    putStrLn "nodes created"
    return (p',r'))
  \>>= \\(p,r) \-> return p
main \= do
  p <- mk2nodes
  Node x q <- readIORef p
  print x
  Node y \_ <- readIORef q
  print y

{-# LANGUAGE RecursiveDo #-}
import Data.IORef
data Node \= Node Int (IORef Node)
mk2nodes \= mdo
  p <- newIORef (Node 0 r)
  r <- newIORef (Node 1 p)
  putStrLn "nodes created"
  return p

main \= do
  p <- mk2nodes
  Node x q <- readIORef p
  print x
  Node y \_ <- readIORef q
  print y

### Lazy algorithm interleaved with effects

A binary tree (immutable) with numbers at internal nodes is given. Replicate the tree but replace the numbers by their sum. Example in ASCII art (4+3+5+1=13) (leaves do nothing and are not shown):

given:     answer:
  4         13
 / \\        / \\
3   5      13 13
     \\          \\
      1         13

Traverse the given tree just once. Moreover, as you traverse the given tree, print it out in some format (in-order format here, like ((3)4(5(1))), but you can modify for pre-order or post-order).

Here is an approach. Given tree *t* and number *s*, `rep_x_sum t s` is written to do two things: replicate *t* but replace the numbers by *s* throughout, and sum up the numbers in *t*. (The name “rep cross sum” means it returns the tuple of the replaced tree and the sum.) This requires just one traversal. Now call this function in such as way as to feed the returned sum back into the parameter *s*, and we will sneak the sum into the replaced tree! Example in ASCII art:

given:     answer:
  4           s
 / \\         / \\
3   5      (s   s  , 13)
     \\           \\
      1           s
           where s=13

So far this can be written in pure code, needing no Monad or MonadFix. But we also want to print something inside the algorithm, which brings in the IO Monad or the Writer Monad (this example uses IO); and to feed a return value back into a parameter in this monadic algorithm, we need MonadFix.

with rec-syntax

with mfix

with mdo-syntax

{-# LANGUAGE RecursiveDo #-}
data BTree \= Z | B Int BTree BTree deriving Show
repsum t \= do
    rec (u,s) <- rep\_x\_sum t s
    putStrLn ""
    return u

rep\_x\_sum Z \_ \= return (Z, 0)
rep\_x\_sum (B i l r) s \= do
  putStr "("
  (l',sl) <- rep\_x\_sum l s
  putStr (show i)
  (r',sr) <- rep\_x\_sum r s
  putStr ")"
  return (B s l' r', i + sl + sr)
main \= repsum (B 4 (B 3 Z Z) (B 5 Z (B 1 Z Z)))
       \>>= print

import Control.Monad.Fix
data BTree \= Z | B Int BTree BTree deriving Show
repsum t \= mfix (\\ ~(u,s) \-> do
    (u',s') <- rep\_x\_sum t s
    putStrLn ""
    return (u',s'))
  \>>= \\(u,s) \-> return u
rep\_x\_sum Z \_ \= return (Z, 0)
rep\_x\_sum (B i l r) s \= do
  putStr "("
  (l',sl) <- rep\_x\_sum l s
  putStr (show i)
  (r',sr) <- rep\_x\_sum r s
  putStr ")"
  return (B s l' r', i + sl + sr)
main \= repsum (B 4 (B 3 Z Z) (B 5 Z (B 1 Z Z)))
       \>>= print

{-# LANGUAGE RecursiveDo #-}
data BTree \= Z | B Int BTree BTree deriving Show
repsum t \= mdo
    (u,s) <- rep\_x\_sum t s
    putStrLn ""
    return u

rep\_x\_sum Z \_ \= return (Z, 0)
rep\_x\_sum (B i l r) s \= do
  putStr "("
  (l',sl) <- rep\_x\_sum l s
  putStr (show i)
  (r',sr) <- rep\_x\_sum r s
  putStr ")"
  return (B s l' r', i + sl + sr)
main \= repsum (B 4 (B 3 Z Z) (B 5 Z (B 1 Z Z)))
       \>>= print

Note that the only laziness needed is just in the parameter `s` to the `rep_x_sum` algorithm. Increasing strictness in `s` will break it, for example this change:

  (r',sr) <- rep\_x\_sum r $! s

but increasing strictness in other things will not (make some other changes to see).

If the given tree is mutable, we can choose to change the numbers in-place instead of building a new tree. Do not worry about race conditions of not knowing whether a number read is old or new — the first thing we do when we visit a node, we read its number to a variable name, so it is the old number; subsequently no matter what we do to the node, that variable name still refers to the old number, so we can rely on it. (For simplicity, only the numbers are mutable here, and the tree shape is immutable.)

with rec-syntax

with mfix

with mdo-syntax

{-# LANGUAGE RecursiveDo #-}
import Data.IORef
data BTree \= Z | B (IORef Int) BTree BTree
repsum t \= do
    rec s <- rep\_x\_sum t s
    putStrLn ""
    return ()

rep\_x\_sum Z \_ \= return 0
rep\_x\_sum (B ref l r) s \= do
  i <- readIORef ref
  writeIORef ref s
  putStr "("
  sl <- rep\_x\_sum l s
  putStr (show i)
  sr <- rep\_x\_sum r s
  putStr ")"
  return (i + sl + sr)
main \= do
  r4 <- newIORef 4
  r3 <- newIORef 3
  r5 <- newIORef 5
  r1 <- newIORef 1
  let t \= (B r4 (B r3 Z Z) (B r5 Z (B r1 Z Z)))
  repsum t
  repsum t

import Control.Monad.Fix
import Data.IORef
data BTree \= Z | B (IORef Int) BTree BTree
repsum t \= mfix (\\s \-> do
    s' <- rep\_x\_sum t s
    putStrLn ""
    return s')
  \>> return ()
rep\_x\_sum Z \_ \= return 0
rep\_x\_sum (B ref l r) s \= do
  i <- readIORef ref
  writeIORef ref s
  putStr "("
  sl <- rep\_x\_sum l s
  putStr (show i)
  sr <- rep\_x\_sum r s
  putStr ")"
  return (i + sl + sr)
main \= do
  r4 <- newIORef 4
  r3 <- newIORef 3
  r5 <- newIORef 5
  r1 <- newIORef 1
  let t \= (B r4 (B r3 Z Z) (B r5 Z (B r1 Z Z)))
  repsum t
  repsum t

{-# LANGUAGE RecursiveDo #-}
import Data.IORef
data BTree \= Z | B (IORef Int) BTree BTree
repsum t \= mdo
    s <- rep\_x\_sum t s
    putStrLn ""
    return ()

rep\_x\_sum Z \_ \= return 0
rep\_x\_sum (B ref l r) s \= do
  i <- readIORef ref
  writeIORef ref s
  putStr "("
  sl <- rep\_x\_sum l s
  putStr (show i)
  sr <- rep\_x\_sum r s
  putStr ")"
  return (i + sl + sr)
main \= do
  r4 <- newIORef 4
  r3 <- newIORef 3
  r5 <- newIORef 5
  r1 <- newIORef 1
  let t \= (B r4 (B r3 Z Z) (B r5 Z (B r1 Z Z)))
  repsum t
  repsum t

We call `repsum` twice in main: the purpose of the second call is printing the tree so we see the updates of the first call.

## MonadFix laws

Here are the laws of MonadFix and some implications.

-   purity: `mfix (return . h) = return (fix h)`
    
    `mfix` over pure things is the same as pure recursion. `mfix` does not add any monadic action of its own.
    
-   left shrinking: `mfix (\x -> a >>= \y -> f x y) = a >>= \y -> mfix (\x -> f x y)`
    
    A monadic action on the left (at the beginning) that does not involve the recursed value (here `x`) can be factored out of `mfix`. So `mfix` does not change the number of times the action is performed, since putting it inside or outside makes no difference.
    
-   sliding: if `h` is strict, `mfix (liftM h . f) = liftM h (mfix (f . h))`
-   nesting: `mfix (\x -> mfix (\y -> f x y)) = mfix (\x -> f x x)`
    
    these two laws are analogous to those of pure recursion, i.e., laws of `fix`.
    

[1]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/
[2]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/recursive_do.html#
