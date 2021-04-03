# Software transactional memory

http://book.realworldhaskell.org/read/software-transactional-memory.html

In the traditional threaded model of concurrent programming, when we share data among threads, we keep it consistent using *locks*, and we notify threads of changes using *condition variables*. Haskell's `MVar` mechanism improves somewhat upon these tools, but it still suffers from all of the same problems.
- Race conditions due to forgotten locks
- Deadlocks resulting from inconsistent lock ordering
- Corruption caused by uncaught exceptions
- Lost wakeups induced by omitted notifications

These problems frequently affect even the smallest concurrent programs, but the difficulties they pose become far worse in larger code bases, or under heavy load. For instance, a program with a few big locks is somewhat tractable to write and debug, but contention for those locks will clobber us under heavy load. If we react with finer-grained locking, it becomes far harder to keep our software working at all. The additional book-keeping will hurt performance even when loads are light.

Software transactional memory (STM) gives us tools that address most of these problems. We execute a block of actions as a transaction using the `atomically` combinator. Once we enter the block, other threads cannot see any modifications we make until we exit, nor can our thread see any changes made by other threads. These two properties mean that our *execution is isolated*.

Upon exit from a transaction, exactly one thing occurs
- If no other thread concurrently modified the same data as us, all of our modifications will simultaneously become visible to other threads.
- Otherwise, our modifications are discarded without being performed, and our block of actions is automatically restarted.

This *all-or-nothing* nature of an `atomically` block is referred to as *atomic*, hence the name of the combinator. It is similar to transactions in databases.
