# Chapter 28. Software transactional memory

http://book.realworldhaskell.org/read/software-transactional-memory.html

Contents
  - The basics
  - Some simple examples
  - STM and safety
  - Retrying a transaction
    - What happens when we retry?
  - Choosing between alternatives
    - Using higher order code with transactions
  - I/O and STM
  - Communication between threads
  - A concurrent web link checker
    - Checking a link
    - Worker threads
    - Finding links
    - Command line parsing
    - Pattern guards
  - Practical aspects of STM
    - Getting comfortable with giving up control
    - Using invariants


In the traditional *threaded model of concurrent programming*, when we *share data among threads*, we keep it consistent using *locks*, and we notify threads of changes using *condition variables*. Haskell's `MVar` (mutable variable) mechanism improves somewhat upon these tools, but it still suffers from many of the same problems.
- Race conditions due to forgotten locks
- Deadlocks resulting from inconsistent lock ordering
- Corruption caused by uncaught exceptions
- Lost wakeups induced by omitted notifications

These problems frequently affect even the smallest concurrent programs, but the difficulties they pose become far worse in larger code bases, or under heavy load. For instance, a program with a few big locks is somewhat tractable to write and debug, but contention for those locks will clobber us under heavy load. If we react with finer-grained locking, it becomes far harder to keep our software working at all. The additional book-keeping will hurt performance even when loads are light.

## The basics

Software transactional memory (STM) gives us a few simple but powerful tools to address most of these problems. We execute a block of actions as a *transaction* using the `atomically` combinator. Once we enter the block, other threads cannot see any modifications we make until we exit, nor can our thread see any changes made by other threads. These two properties mean that our *execution is isolated*.

Upon exit from a transaction, exactly one of the following things will occur:
1. If no other thread concurrently modified the same data, all of our modifications will simultaneously become visible to other threads.
2. Otherwise, our modifications are discarded without being performed, and our block of actions is automatically restarted.

This *all-or-nothing* nature of an `atomically` block is referred to as *atomic* and is also found in the databases that support transactions.

## Multi-player role game example

In a multi-player role playing game, a player's character has some state (health, possessions, money). To explore STM, we'll start with functions and types related to working with a character's state in a game.

The STM API is provided by the *stm* package, and its modules are in the `Control.Concurrent.STM` hierarchy.

```hs
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoDeriveAnyClass #-}

import Control.Concurrent.STM
import Control.Monad

data Item = Scroll
          | Wand
          | Banjo
          deriving (Eq, Ord, Show)

newtype Gold = Gold Int
    deriving (Eq, Ord, Show, Num)

newtype HitPoint = HitPoint Int
    deriving (Eq, Ord, Show, Num)

type Inventory = TVar [Item]
type Health    = TVar HitPoint
type Balance   = TVar Gold

data Player = Player
    { balance   :: Balance
    , health    :: Health
    , inventory :: Inventory
    }
```

The `TVar` parameterized type is a mutable variable that we can read or write inside an `atomically` block.

To perform a basic transfer of money from one Balance to another, all we have to do is adjust the values in each `TVar`.

```hs
basicTransfer qty fromBal toBal = do
    fromQty <- readTVar fromBal
    toQty   <- readTVar toBal
    writeTVar fromBal (fromQty - qty)
    writeTVar toBal   (toQty + qty)


-- The properties of atomicity and isolation guarantee that if another thread sees a change in bob's balance, they will also be able to see the modification of alice's balance
transferTest :: STM (Gold, Gold)
transferTest = do
    alice <- newTVar (12 :: Gold)
    bob   <- newTVar 4
    basicTransfer 3 alice bob
    liftM2 (,) (readTVar alice) (readTVar bob)


-- pure function to remove an item from the list of player's inventory
removeInv :: Eq a => a -> [a] -> Maybe [a]
removeInv x xs =
    case takeWhile (/= x) xs of
        (_:ys) -> Just ys
        []     -> Nothing

-- transactional function to give an item to another player; complicated by the need to determine whether the donor actually has the item in question
maybeGiveItem item fromInv toInv = do
    fromList <- readTVar fromInv
    case removeInv item fromList of
        Nothing      -> return False
        Just newList -> do
            writeTVar fromInv newList
            destItems <- readTVar toInv
            writeTVar toInv (item : destItems)
            return True
```

## STM and safety

If we are to provide atomic, isolated transactions, it is critical that we cannot either deliberately or accidentally escape from an `atomically` block. Haskell's type system enforces this on our behalf, via the STM monad.
