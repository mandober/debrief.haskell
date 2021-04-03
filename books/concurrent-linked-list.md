# Concurrent Programming

Comparing the performance of concurrent
linked-list implementations in Haskell
2009 Martin Sulzmann, Edmund Lam, Simon Marlow

There is a variety of synchronization models when implementing *shared-state concurrency abstractions*. In Haskell, they range from the very high level (STM) to the very low level (mutable variables with atomic read-modify-write). Each model offers a unique tradeoff between composability, scalability and efficiancy, but knowing which one to use is a skill in itself.

The 3 main synchronization models supported by GHC
* `𝚂𝚃𝙼`   Software Transactional Memory
* `𝙼𝚅𝚊𝚛`  elementary lock-based synchronization primitive
* `𝙸𝙾𝚁𝚎𝚏` and `𝚊𝚝𝚘𝚖𝚒𝚌𝙼𝚘𝚍𝚒𝚏𝚢𝙸𝙾𝚁𝚎𝚏`
   low-level synchronization using mutable variables
   and an atomic read-modify-write operation.

Factors of synchronization model:
- How much *overhead* is caused by each synchronization model in GHC
- How well does each synchronization model *scale* with the number of CPU cores
- How many CPU cores are needed to *out-perform* a sequential implementation
- Which *other aspects* may influence the choice of synchronization model

## Synchronization Primitives in GHC

The three basic models of concurrent synchronization available in GHC: STM at the highest-level, mutable vars with atomic read-modify-write at the lowest-level.

## STM

STM was added to GHC in 2005 as a way to program concurrent synchronization in a way that is composable, in the sense that operations that modify shared state can safely be composed with other operations that also modify shared state.

The basic data object is the transactional variable, `TVar`. A *transaction* is a computation performed over a set of TVars, yielding a result; each transaction is performed atomically.

The implementation technique that makes transactions viable is *optimistic concurrency*, that is, all transactions run to completion under the assumption that no conflicts have occurred, and only at the end of the transaction do we perform a *consistency check*, retrying the transaction from the start if a conflict has occurred.

This is "optimistic" in the sense that it performs well if conflicts are rare, but poorly if they are common. If conflicts are common (many transactions modifying the same state), then optimistic concurrency can have worse performance that just sequentializing all the transactions using a single global lock.

What's important is that a transaction can only modify `TVar`s, because if the transaction is found to be in conflict, then its effects must be discarded, which is only possible if the effects are restricted to a known class of undoable effects. In Haskell, we can restrict effects using the type system, thus we have the `STM` monad whose only stateful effects are those that affect TVars.

```hs
import Control.Monad.STM
-- https://hackage.haskell.org/package/stm-2.5.0.0/docs/Control-Monad-STM.html
newTVar    :: a -> STM (TVar a)
readTVar   :: TVar a -> STM a
writeTVar  :: TVar a -> a -> STM ()
retry      :: STM ()
atomically :: STM a -> IO a
```

Here is a simple STM example to model an "atomic" bank transfer.

```hs
transfer :: TVar Int -> TVar Int -> Int -> IO ()
transfer fromAcc toAcc amount = atomically $ do
    f <- readTVar fromAcc
    if f <= amount
    then retry
    else do
        writeTVar fromAcc (f - amount)
        t <- readTVar toAcc
        writeTVar toAcc (t + amount)
```

We transfer amount currency units from fromAcc to toAcc. If the balance of fromAcc is insufficient we simply retry, that is, we abort the transaction and try again. There is no point in re-running the transaction if fromAcc has not changed. Hence, the transaction simply blocks until fromAcc has been updated.

To summarize, programming with STM has two advantages. It is straightforward to compose smaller STM computations such as `writeTVar fromAcc (f - amount)` with other STM computations to form a larger STM computation (the actual bank transfer).

Furthermore, the programmer does not need to worry in which order to acquire (lock) the two accounts since STM computations are executed optimistically. For our example, this means that if there are two concurrent transfers involving the same set of accounts:
* neither transaction will block the other, instead,
* the STM run-time optimistically executes both transactions but only one of them can commit and the other is retried.

Thus, STM avoids common pitfalls when programming with locks where the programmer has to acquire resources in a specific order to avoid *deadlocks*.

## MVars

The `MVar` is an elementary *lock-based* synchronization primitive in Haskell. This synchronization method has the following operations:

```hs
newMVar      :: a -> IO (MVar a)
newEmptyMVar :: IO (MVar a)
takeMVar     :: MVar a -> IO a
putMVar      :: MVar a -> a -> IO ()
```

An `MVar` is like a one-place channel: it can be either full or empty. The `takeMVar` operation returns the value if the MVar is full or blocks if the MVar is empty, and `putMVar` fills the MVar if it is empty or blocks otherwise.

It is possible to implement the semantics of `MVar` using `STM`, although without some of the useful operational properties of the native implementation.

Here is the `STM` implementation of `MVar` and `takeMVar`:

```hs
newtype MVar a = MVar (TVar (Maybe a))

takeMVar :: MVar a -> IO a
takeMVar (MVar tv) = atomically $ do
    m <- readTVar tv
    case m of
        Nothing -> retry
        Just a -> do
            writeTVar tv Nothing
            return a
```

This is a reasonable implementation of `takeMVar` in that it has the same blocking behavior as the native implementation: the same set of programs will deadlock with this implementation as would with the native implementation.

However, the STM implementation is less useful in two ways:

* *fairness*: The native implementation of `MVar` holds blocked threads in a FIFO queue, so a thread is never blocked indefinitely as long as the `MVar` is being repeatedly filled (or, respectfully, emptied).

* *single-wakeup*: When there are multiple threads blocked in `takeMVar` on a single MVar, and the MVar is filled by another thread, then the STM impl will wake up all the blocked threads. In this case we know that only one of these threads will succeed in its blocked operation and the others will all block again, but the STM implementation in the runtime isn't aware of this property and has to assume that any subset of the blocked transactions can now succeed. On the other hand, *the native MVar impl will wake up only one thread*, and hence will scale more effectively when there is high contention for an MVar.


In this paper, we use `MVar` primarily to implement the combination of a *mutex* and a *mutable variable*.
- taking the MVar is equivalent to acquiring the lock and reading the var
- filling the MVar is equivalent to writing the var and releasing the lock

As with traditional mutexes, when taking multiple MVars, we must be careful to take them in a consistent order, otherwise multiple threads trying to take an overlapping set of MVars may deadlock.


## IORef and atomicModifyIORef

This is the lowest level synchronization method available in GHC that closely corresponds to *compare-and-swap-style* operations in other languages.

An `IORef` is a **mutable variable**, with the following basic operations that correspond to creation, reading and writing.

```hs
newIORef   :: a -> IO (IORef a)
readIORef  :: IORef a -> IO a
writeIORef :: IORef a -> a -> IO a
```

The memory model for IORefs is notoriously hard to pin down correctly; for example, *are writes made by one thread always observed, in order, by another thread?* Attempts to nail down these issues have lead to significant complexities in other language definitions (e.g. in C++ and Java).

> In Haskell, we currently use the **model of complete sequential consistency**: all writes are observed in the order they are performed.

It turns out that this doesn't imply any significant burden on the compiler beyond what the compiler already has to do to ensure safety; namely, that when a thread calls `readIORef` and then inspects the object returned, the object it reads is actually present.

For example, if writes were not ordered in the underlying machine, we might see the write to the `IORef` but not the writes that create the object that it points to.

Implementing this safety guarantee implies the use of *memory barriers* on some types of processor, and typically such memory barriers will also provide *sequential consistency* for `readIORef` and `writeIORef`.

Still, `readIORef` and `writeIORef` alone are not enough to program most concurrent algorithms, normally we require at least an *atomic read-modify-write operation*. In Haskell, such operation is:

```hs
atomicModifyIORef :: IORef a -> (a -> (a,b)) -> IO b
```

The behavior of `atomicModifyIORef` can be understood as being equivalent to the following:

```hs
atomicModifyIORef r f = do
    a <- readIORef r
    let p = f a
    writeIORef r (fst p)
    return (snd p)
```

with the important exception that the whole operation is performed *atomically* with respect to other threads.

The reason it can be performed atomically is Haskell's lazy evaluation: the function `f` is not actually called during `atomicModifyIORef`. Both the contents of the `IORef` and the value returned are *thunks* (also known as *suspensions*) that will demand the value of `r`, and hence call `f`, when either of their values are demanded.

The implementation of `atomicModifyIORef` in the runtime system looks much like the definition above, except that the final `writeIORef` is replaced by a *compare-and-swap*: if the current value of the `IORef` is still `a`, then replace it with `fst r`, otherwise loop back to the `readIORef` again.

In some ways this is like a mini transaction over a single variable, and indeed in one of the examples we consider later we do use STM to model `atomicModifyIORef` in order to make a direct comparison between the two.

In contrast to MVars, `atomicModifyIORef` can be used to implement non-blocking algorithms, in the sense that a thread can always make progress even if many other threads in the system are stalled.

STM also has non-blocking properties, but it suffers from other pathologies, such as *discrimination against long-running transactions* (of course, steps can be taken to mitigate these effects).


## Case Study: Concurrent Singly-Linked Lists

We consider a standard singly-linked list structure where we use sentinel nodes to represent the head and the tail of the list.

```hs
import Foreign.Ptr

data List a = Node { val :: a, next :: Ptr (List a) } | Null

data ListHandle a = ListHandle
    { headList :: Ptr (Ptr (List a))
    , tailList :: Ptr (Ptr (List a))
    }

-- The list supports the following functionality:
newList   :: IO (ListHandle a)
addToTail :: ListHandle a -> a -> IO (Ptr (List a))
find      :: Eq a => ListHandle a -> a -> IO Bool
delete    :: Eq a => ListHandle a -> a -> IO Bool
```

* `newList` creates an empty new list.
* We consider unsorted linked lists, so `addToTail` inserts an element by adding it to the tail, returning a reference to the newly added node.
* `find` traverses the list starting from the head node and searches for a specified element, until the element is found or the tail of the list is reached.
* `delete` removes an element from the list.

Our lists are not ordered, and neither do we provide an operation to insert an element anywhere except at the end. It would be possible to change our algorithms to support ordered lists with insertion, although that would certainly add complexity and would not, we believe, impact our results in any significant way.

In the following, we consider several concurrent implementations where we replace `PTR` by `TVar`, `MVar` and `IORef`.

The challenge is to avoid inconsistencies by protecting *critical sections*.Without any protection (synchronization), concurrent deletion of elements 2 and 3 can lead to a list where element 3 is still present. In turn, we consider several approaches to preventing this from happening.

```
Initial list:

1 ----> 2 ----> 3 ----> 4

Concurrent operations: delete 2 || delete 3

One possible execution that gives rise to an inconsistent result:

Step 1: delete 2
 ______________
/              ↘
1       2 ----> 3 ----> 4

Step 2: delete 3
 ______________
/              ↘
1       2       3 ----> 4
        \______________↗
```

## Lock-free linked list using STM

### Straightforward Version

The linked list is of the following form.

```hs
data List a
    = Null
    | Node
    { val  :: a
    , next :: TVar (List a)
    }
    | Head { next :: TVar (List a) }
```

We use *shared pointers*, `TVars`, to synchronize the access to shared nodes. To ensure consistency we simply place each operation in an *STM transaction*.

The complete definition of all 4 functions revelas no surprises - we can effectively copy the code for a sequential linked list but we only need to execute the entire operation inside an `atomically` statement.

We straightforwardly obtain a correct implementation of a concurrent linked list, however, this impl has severe performance problems: the main problem is that because each transaction encompasses the entire traversal of the list, any transaction which modifies the list will conflict with any other transaction on the list.

This implementation does not have the *non-interference properties* that we desire, namely that *multiple threads should be able to update different parts of the list concurrently*.

### Dissecting Transactions
