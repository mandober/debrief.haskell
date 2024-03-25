# Enecuum. Framework possibilities

> In the previous article, we’ve discussed why we have chosen Haskell for our blockchain framework. In this article, we’ll demonstrate you…

[

![Enecuum](https://miro.medium.com/fit/c/56/56/1*4zFiKZtvGJjh2bollyfqwA.png)



](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/?source=post_page-----d4fa49c3ea40--------------------------------)

![](https://miro.medium.com/max/5012/1*gtElrjWGsMqFw76IiEyzSA.png)

In the previous article, we’ve discussed why we have chosen Haskell for our blockchain framework. In this article, we’ll demonstrate you what makes the framework so specific comparing to other solutions including those already written in Haskell.

The goal of the Enecuum.Framework is to make writing of blockchain algorithms and behavior simple. We want our blockchain code to obey the following requirements:

*   Read, write and update the logic of nodes interaction easily.
*   Decrease the complexity of writing concurrent data models and parallel code to the minimum.
*   Create APIs and protocols without much boilerplate.
*   Provide a high-level language for nodes interaction logic that should be understandable without much knowing of Haskell.
*   Able to test nodes’ behavior both in real and special test environments.
*   Control code quality and provide the best project maintainability possible.

So, what type of blockchain nodes we can write with this framework? Well, not only “blockchain nodes”: it’s possible to create any distributed actors communicating with each other. The framework doesn’t lock us on the blockchain domain only. To be more specific, the framework is suitable, if you need a node that:

*   acts within a real network via TCP, UDP or JSON-RPC;
*   processes an arbitrary concurrent state and makes many computations in parallel;
*   works with arbitrary structure of data graph (the graph is also concurrent and safe);
*   can handle external requests of arbitrary type;
*   can have a convenient console client, and adding of new commands should not be hard;
*   has some non-trivial business logic you want to test separately from other behavior;
*   works with some database storage.

This is not a complete list of possibilities, but it’s enough to create a very different type of applications with our framework.

Let’s create two nodes who will play Ping-Pong via network forever. Consider the following code listing: it has two nodes defined. One of the nodes will be a TCP server, and another will be a TCP client.

![](https://miro.medium.com/max/2000/1*YSOYynySbo31bR26fVW47A.png)

So, this is all the code needed to create client and server, except the handlers. We create handlers to react on messages somehow. The following script shows the handlers:

![](https://miro.medium.com/max/2000/1*wcirSckTq-OKW81bZnnyJA.png)

That’s all. The handlers are very simple. The handler receives a “back to the caller” connection, so every time it catches a “Ping n” or “Pong n” message, it immediately sends the accompanying message with the counter increased by 1.

This example demonstrates how simple it is to write full-fledged client-server operations. But what about stateful calculations and parallel processes? Let’s see.

Currently, our nodes will act forever because we called a special function, **awaitNodeForever**. But suppose, we want our Ping-Pong nodes

either stop when the counter reaches some threshold, or when some time passes. This sounds like a concurrency task. How we can do it?

*   We won’t be using **awaitNodeForever**.
*   The node will wait on some boolean signal until it becomes True. After that, node should finish.
*   To keep a signal, we’ll create a concurrent state variable, **signal**, with the initial value **False**.
*   We’ll start a separate process in each node that will set the signal to **True** when some time passes.
*   In handlers, we’ll be checking if the counter is greater than some threshold. If so, we’ll set the signal to True.

Therefore, the code of nodes will change as follows:

![](https://miro.medium.com/max/2000/1*MaNPreWKHYG4L3Se1V_K7A.png)

So, nodes are now creating a variable for signaling. This variable can be accessed from multiple threads concurrently, and there is a way to wait for a particular state of the **signal** variable. In the following code listing, you can see a special combinator **retry** that will block the evaluation when the state of the **signal** variable is **False**.

![](https://miro.medium.com/max/2000/1*slN0ii67_98WiMvxKav8dw.png)

The code in the **waitSignal** function will automagically resume after blocking when the state of the **signal** variable changes. It’s not needed to poll the variable here, all the synchronization will happen in the framework engine.

Finally, we need to see how the handlers changed. Not so much. Here:

![](https://miro.medium.com/max/2000/1*E2NNVc5LHu0BwpNzODNRcQ.png)

So, we just check the **i** counter when it becomes 3 or greater. In that case, we write **True** into the signal. This is all we need to work with concurrent state. No mutexes, no critical sections, no other ways to synchronize the handlers and the internal processes of the node. Working with concurrent variables becomes much simpler and more safe. With the framework, writing blockchain protocols becomes really fast and convenient.

In this article, we considered only a small part of the Enecuum.Framework. In the next articles, we’ll tell more about other possibilities such as graph building, console client, logging, getting random values and cryptography.


[Source](https://enqblockchain.medium.com/enecuum-framework-possibilities-d4fa49c3ea40)