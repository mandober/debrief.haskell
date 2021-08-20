# Enecuum.Framework Possibilities, Part 2

> In the previous article, we were talking about basic features of the framework. We’ve considered the following topics:

[

![Enecuum](https://miro.medium.com/fit/c/56/56/1*4zFiKZtvGJjh2bollyfqwA.png)



](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/?source=post_page-----7c8ff65c1c4e--------------------------------)

![](https://miro.medium.com/max/5012/1*2mrw6n5coATWZTQTUNo7qw.png)

In the previous article, we were talking about basic features of the framework. We’ve considered the following topics:

*   What the framework can be used for.
*   How to write scenarios for nodes that interact using TCP-like connection.
*   How to manage a mutable and concurrent state for node.

It’s time to discuss more useful features we need to build a complex node logic and behavior.

In the start, let me revise the way the node can be either server or client for TCP interaction. In the following code, we create a TCP server node that accepts some external connection and expects a **Ping** data structure to be sent from the client. It also sends a **Pong** message back to the client after successful **Ping** processing:

![](https://miro.medium.com/max/2000/1*fYMYFcSB2E_Iv38U_X7RgA.png)

There is nothing new in the code above comparing to the samples in the previous article except the two small things:

*   **awaitNodeForever**: this action makes the node acting forever. It’s also possible to have another conditions of node finishing including conditions by time or by event. We won’t be focusing on this in the future.
*   Type declarations for functions.

The type declarations aren’t mandatory, but it’s much better to see the actual types to have a complete view. Thus, **server** has type **NodeDefinitionL ()** which means it’s a main script of the node: exactly like the **main** function in any other programming languages. The handler has type **NodeL ().** It can’t do many things a **NodeDefinitionL** function can, and this division makes it possible to specify responsibilities for different parts of the node. It leads to more safety and convenience for a developer because he is saved from doing wrong things which don’t make sense and the moment. For example, a handler can’t start own TCP server, or it can’t make node wait forever: this doesn’t make sense. So, the Enecuum.Framework has a good separation of functionality due to different domain specific languages nested into each other. This is an interesting theme, and we’ll return to it in the next articles.

Having a single handler doesn’t seem that useful, right? It should be possible to specify as many API calls as needed, because the most nodes will have a much wider APIs. Let’s expand our example and imagine the node should be a log collecting server. It should receive external logs all the time and put them into a memory storage. Ideally, it should store messages into DB, but we’ll stay with a memory storage only to keep the article focused.

The task is very common and has several different parts. We’ll approach it step by step. The steps will be:

1.  Define a type for log entries able to be sent via network.
2.  Create a data storage for logs in memory.
3.  Create a handler for processing of this type messages.
4.  Define new node API.
5.  Create a sample client node.

**Step 1. Define a type for log entries.**

This is not a big deal. Our log entry will have two fields: **level** and text **message**.

![](https://miro.medium.com/max/2000/1*2629gOTdrw60JZvWMqfCSg.png)

**Step 2. Create a data storage for logs in memory.**

Now we need a data storage for log entries. In the simplest case, it will be a linear list of **LogEntry** values. The node should be able to change the list, so we need a mutable state variable:

![](https://miro.medium.com/max/2000/1*UXepVruR8TGHKHcfsUtn4g.png)

The **StateVar** type is thread-safe and concurrent. It can be changed from different places without risk to catch deadlock. In the framework, **StateVar** is the only way to have a safe mutable concurrent state. There are some special properties of its using and most of them correspond to the **Software Transactional Memory** concept, and to know more you can get familiar with Haskell **STM** library.

**Step 3. Create a handler for processing of this type messages.**

In the handler, we need to catch a log entry and store it into **LogVar**. We probably don’t want to mix these two actions, so we make them to be separate functions:

![](https://miro.medium.com/max/2000/1*co4t5wq9ugQTB4BZ_oZ2uw.png)

Notice that the **addLogEntry** works inside the **StateL** langauge. This means that every variable operation in the **addLogEntry** function will be made transactionally: while the function is working, no other thread can change the **logVar**. If so happens that the function is still working, other threads will be blocked to wait theirs turn to change the variable. The **atomically** function is used to run a **StateL** action inside a **NodeL** action.

**Step 4. Define new node API.**

Now, we can add this handler into the node definition… But why should we limit ourselves by TCP only? Can we provide both TCP and UDP interfaces? Sounds reasonable. And this is simple to do! Look:

![](https://miro.medium.com/max/2000/1*FgdPiJGwTX2wbj6NXrIAPQ.png)

And that’s pretty much it. When the node starts, it will also start two servers on ports 2000 and 2001. We can use the same handlers here and there, and this looks cool, isn’t it?

**Step 5. Create a sample client node.**

Finally, we need a client node. It should send several log messages to the server. Not very inspiring, you’d say? I agree. Let’s expand this task!

Suppose, the client node should read a text message from the user from the standard input. After that, it sends the message to the server. Also, let it print the fact the message is sent. This is a typical set of requirements for a CLI application and the Enecuum.Framework supports these requirements well. In the following code listing a handler for console is defined: it will be called on the corresponding user’s input.

![](https://miro.medium.com/max/2000/1*vYGvwweEpW3axYAmZFoDQw.png)

As you can see, the **std** handlers are organized in the same manner as TCP or UDP handlers, so you can setup several handlers for different user commands. That’s very convenient and useful.

To complete the example, look at the output the user will see after node starts (well, not exactly this output, but something similar to it):

![](https://miro.medium.com/max/2000/1*i_UjUHb2L-dNmWP6WuPGCQ.png)

We’ve considered a lot of important features of the framework. TCP, UDP, console interaction, logging and concurrent state. But this is not the end. There are many more features to be discussed: database support, configs, running of nodes, file system work, graph, random numbers generation and many others, so stay tuned!


[Source](https://enqblockchain.medium.com/enecuum-framework-possibilities-part-2-7c8ff65c1c4e)