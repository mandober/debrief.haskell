# Building network actors with Node Framework

Article topics:

  * [What is Node Framework?](#what-is-node-framework)
  * [Sample nodes](#sample-nodes)
  * [Node Framework languages structure](#node-framework-languages-structure)
    - [Core languages](#core-languages)
    - [Framework languages](#framework-languages)
  * [Node Framework design and architecture](#Node-Framework-design-and-architecture)
    - [The Free monad approach](#The-Free-monad-approach)
    - [Creating a Free monad language](#Creating-a-Free-monad-language)

# What is Node Framework?

The aim of the [Node](https://github.com/graninas/Node) project is to provide a set of tools for building of network acting nodes and blockchain protocols. Nodes are capable to handle concurrent state (with `STM`), work with KV database, manage any types of graph (which is also concurrent). The code of the nodes should be testable, safe and well-maintainable, and writing of blockchain protocols should be as easy as possible. It allows you to create `TCP`, `UDP` and `JSON-RPC` APIs (both client and server side), intractable command line applications, or any kind of stateful applications. Under some circumstances, it can be a replacement for [Cloud Haskell](http://haskell-distributed.github.io/)! Certainly, the framework provides more necessary things like logging, threading, config management, and many other useful bits and pieces you might need for your specific use cases.

The framework is based on the ideas I've described in my halfbook ["Functional Design and Architecture"](https://github.com/graninas/Functional-Design-and-Architecture). The architecture of the framework is a set of Free monadic domain specific languages (eDSLs). Currently, there are about 15 different Free monad eDSLs organised hierarchically. Nodes you write will be interpretable monadic scripts that are easy to read and test. The project contains several sample nodes performing some blockchain activity, and there are also integration tests on this logic. The project is also well organised and has a good test coverage.

# Sample nodes
Let me present you some example of Node framework usage. There will be a `UDP server` and two identical clients. The clients will be sending a `Ping` message periodically to the server. When the server receives a `Ping` message, it increments a counter and sends the state of the counter back to the client as a `Pong` message. The server should stop after a predefined count of iterations.

Firstly, consider the server code. We can define node to be a network server on `TCP`, `UDP` or `JSON-RPC` protocol. We declare that the server should listen on a specific port and should react to a concrete messages by starting some logic every time a message comes. We place this logic into a set of handlers and so we define the API of the server. In the following listing `pingServerNode` defines a server node and `acceptPing` is a handler of the `Ping` message.

```haskell
-- Messages
newtype Ping = Ping Text deriving (Generic, ToJSON, FromJSON)
newtype Pong = Pong Int  deriving (Generic, ToJSON, FromJSON)

-- Ping server definition node.
-- Gets node config data type, we'll see it a bit later.
pingServerNode :: NodeConfig PingServerNode -> NodeDefinitionL ()
pingServerNode cfg = do
    let threshold = _stopOnPing cfg
    let port      = _servingPort cfg

    -- Creation of concurrent variables (TVars)
    pingsCount <- newVarIO 0
    status     <- newVarIO NodeActing

    -- Starting a separate process for serving on UDP port.
    serving Udp port $
        -- Handler of the particular network message (see below)
        handler $ acceptPing status pingsCount threshold
    
    -- Node will be alive while @status@ is @NodeActing@
    awaitNodeFinished' status

-- Handling Ping messages.
acceptPing
    :: StateVar NodeStatus
    -> StateVar Int
    -> Int
    -> Ping
    -> Connection Udp
    -> NodeL ()
acceptPing status pingsCount threshold (Ping clientName) conn = do
    pings <- atomically $ do
        modifyVar pingsCount (+1)
        readVar pingsCount

    let done = pings + 1 >= threshold
    when done $ do
        close conn
        writeVarIO status NodeFinished
        logInfo $ "Pings threshold reached: " +|| threshold ||+ ". Finishing."

    unless done $ do
        send conn (Pong pings)
        logInfo $ "Ping #" +|| pings ||+ " accepted from " +|| clientName ||+ "."
```

In fact, a node can serve many different APIs at the same time. In the `NodeDefinitionL` language we can fork parallel processes, define a console API that will be processing commands from the user, work with state, logs, randomness, time and so on. The `NodeL` language is a bit more restrictive. You can't do some things there like starting processes or declare servers. On of the motivation of this is to separate responsibilities and disallow making potentially bad things (although there are another pitfalls that are hard to make impossible).

A client node code looks similar. The client opens a connection to the server and starts a process that will send messages periodically.

```haskell
-- Accepting pong responses from the server.
acceptPong :: Pong -> connection -> NodeL ()
acceptPong (Pong pingsCount) _ =
    logInfo $ "Pong accepted from server. Pings count: " <> show pingsCount

-- Sending pings to the server.
pingSending :: StateVar NodeStatus -> NodeConfig PongClientNode -> Connection Udp -> NodeL ()
pingSending status cfg conn = do
    delay $ _pingDelay cfg
    logInfo "Sending Ping to the server."
    eSent <- send conn (Ping $ _clientName cfg)
    case eSent of
        Right () -> pingSending status cfg conn
        Left _   -> do
            logInfo "Server is gone."
            close conn
            writeVarIO status NodeFinished

-- Pong client definition node.
pongClientNode :: NodeConfig PongClientNode -> NodeDefinitionL ()
pongClientNode cfg = do
    status <- newVarIO NodeActing

    -- Connecting to the server.
    mbConn <- open Udp (_pingServerAddress cfg) $
        handler acceptPong

    case mbConn of
        Nothing -> logError "Ping Server not found"
        Just conn -> do
            -- Forking separate process of periodical pings.
            process (pingSending status cfg conn)
            -- Waiting when the node is finished.
            awaitNodeFinished' status
```

The languages don't expose any unnecessary details like how to fork threads for servers and how to process messages. There is nothing about logging implementation and where logs should go, and it's not needed to know how the concurrent state is made. You just write a pure business logic and express what your algorithms are, but not how they work inside. Due to `Free` monads, we can hide all the implementation details. By running these scripts in different environments with different interpreters we can test our logic easily.

So how do we run server and client nodes? Before that, let's add missing things, namely - node definition types. We should create three types: node unique tag type, node config and node definition type. The server node types (instances are not presented here, you can find them in the full example on [here](https://github.com/graninas/Node/tree/master/src/Enecuum/Assets/Nodes/TstNodes/PingPong)):

```haskell
-- Node unique tag.
data PingServerNode = PingServerNode

-- Node config.
data instance NodeConfig PingServerNode = PingServerNodeConfig
    { stopOnPing  :: Int
    , servingPort :: PortNumber
    }

-- Node definition type.
instance Node PingServerNode where
    data NodeScenario PingServerNode = PingServer
    getNodeScript PingServer = pingServerNode
    getNodeTag _ = PingServerNode
```

The node definition type can seem a bit cryptic, but don't worry about that. I just say it allows to specify several scenarios for the same node. In here, there is only one scenario, `PingServer`, and its has been mapped to the `pingServerNode'` script. The JSON config file will look like the following:

```json
{
    "node": "PingServerNode",
    "nodeScenario": "PingServer",
    "nodeConfig": {
        "tag": "PingServerNodeConfig",
        "stopOnPing": 200,
        "servingPort": 3000
    },
    "loggerConfig": {
        "logFilePath": "",
        "format": "$prio $loggername: $msg",
        "logToFile": false,
        "logToConsole": true,
        "level": "Debug"
    }
}
```

The framework allows you to specify logger targets: console, file or both. You can choose logging level and format.

After compiling the code and starting the `enq-test-node-haskell` executables with server and client configs you'll see the following outputs:

```bash
$ enq-test-node-haskell singlenode ./configs/tst_ping_server.json

Starting node...
    Node:     PingServerNode
    Scenario: PingServer
INFO Node.Main: Ping #1 accepted from "Pong client #1".
INFO Node.Main: Ping #2 accepted from "Pong client #2".
INFO Node.Main: Ping #3 accepted from "Pong client #1".
INFO Node.Main: Ping #4 accepted from "Pong client #2".
```
```bash
$ enq-test-node-haskell singlenode ./configs/tst_pong_client1.json
 
Starting node...
    Node:     PongClientNode
    Scenario: PongClient
INFO Node.Main: Sending Ping to the server.
INFO Node.Main: Pong accepted from server. Pings count: 1
INFO Node.Main: Sending Ping to the server.
INFO Node.Main: Pong accepted from server. Pings count: 3
```
```bash
$ enq-test-node-haskell singlenode ./configs/tst_pong_client2.json
 
Starting node...
    Node:     PongClientNode
    Scenario: PongClient
INFO Node.Main: Sending Ping to the server.
INFO Node.Main: Pong accepted from server. Pings count: 2
INFO Node.Main: Sending Ping to the server.
INFO Node.Main: Pong accepted from server. Pings count: 4
```

These ping-pong nodes are quite simple and aren't presenting all of the framework possibilities. If you want more samples, you can try our [test nodes](https://github.com/graninas/Node/tree/master/src/Enecuum/Samples/Assets/Nodes/TstNodes).

# Node Framework languages structure
Now, let's slightly open the door into the internals of the framework. Because the key to its functionality is a set of Free monad languages, we'll need to uncover what languages we have. There are two kinds of languages: the Core languages and the Framework languages.

### Core languages
These languages represent some real subsystem.

- `HGraphL` - Working with generic any structure graph (concurrently).
- `StateL` - Working with concurrent state variables. Represents a wrapper around native STM.
- `DatabaseL` - Raw KV database interface. RocksDB is the implementation currently.
- `LoggerL` - Logging possibilities.
- `FileSystemL` - Working with file system.
- `RandomL` - Random generation and crypto methods.
- `CryptoL` - Subset to work with crypto methods.
- `TimeL` - Getting current time.
- `ControlFlowL` - Controlling the flow of the evaluation. It has the only method for thread delaying currently.

### Framework languages
These languages allow to build nodes. The most of the node logic will be written in these two languages.

- `NodeDefinitionL` - Language to define servers, APIs for node, command line methods. Provides methods for parallel process forking (forkIO essentially).
- `NodeL` - Allows to work with connections (`TCP`, `UDP`), create graphs and databases, evaluate scripts in core languages. Also, has methods to evaluate database and state scripts.

The languages are organised hierarchically. For example, it's possible to call a `NodeL` script from the `NodeDefinitionL` one, but not vice versa. This prohibits some unwanted behaviour like starting servers in network handlers. The following diagram shows the current structure of the languages.

![image](https://user-images.githubusercontent.com/828659/49463610-754c0a80-f82b-11e8-8c72-eb261fc9b445.png)

There are several moments here to note.

* We make hierarchy of the languages by nesting one into a method of another. For example, `EvalNode` in `NodeDefinitionL` or `EvalDatabase` in `NodeL`. We'll see how this woks in the next section.
* `CoreEffectL` is a "container" language. It does not expose own logic but provides a single entry point into the underlying subsystems.
* `StateL` and `HGraphL` languages allow you to work with concurrent state. Under the hood, these methods are translated into methods in the `STM monad`. But we need to control the transactions from our logic, so we have the `EvalStateAtomically` method in `NodeL`. As you can see, the `HGraphL` language is referenced from the `StateL` language. This means working with node state variables and graph parts can be done inside a single transaction which makes possible to update node state and graph simultaneously and thread safe.
* There is also `EvalGraphIO` method. It evaluates parts of the graph script separately from each other (not transactionally). This can be useful to make some dirty reads from the graph.
* It's possible to have several different graphs and databases.
* The structure is not yet finished, so we can move or change some languages in the future.

Now it's time to discuss more technical topics about the ideas used in the framework.

# Node Framework design and architecture
### The Free monad approach

Free monadic language represents an interface to some subsystem. Using this interface, we create monadic scripts like `pingServerNode` and `pongClientNode`. Due to the nature of the `Free monad`, we need to interpret these scripts in order to make them really work. We create interpreters for every language we have, and pattern-match over the language methods mapping them onto real actions. Although free monadic scripts limit us to some degree (you can't eval `IO` there and should not do any unsafe things), interpreters can be completely impure because they are just "bridges" from our pure scripts to the outside impure world. It's also possible to have many different interpreters for a single language, and switch them at runtime if needed.

Speaking strictly, the `Free monad design pattern` allows you to separate interface from implementation. It is a pure functional approach to the `Inversion of Control` principle. Possibility to pass implementation under some interface into a client code also know as `Dependency Injection`. This technique is quite often used in the OOP world, and it seems there is a demand on bringing these ideas into functional programming because we need to build big complex applications that should be capable to satisfy different requirements. The Free monad approach can be the answer because it is very powerful and gives you many benefits:

* Accidental complexity reducing. When you separate interface from the implementation, you can write more clean and understandable business logic code. With the Free monad approach, it's quite easy to divide the project code into several layers. (In fact, there is a further development of this idea known as `"Onion Architecture"`).
* Testability of scripts. You can run them against real or test environment by substituting the interpreters. This allows to mock subsystems and unit-test your business logic as you would do it in imperative languages.
* Purifying of the domain logic.  You need to clearly understand what is part of your domain and what is not (can be safely moved into the runtime layer). This is important because when you mix implementation details into business logic, you make it fragile, needlessly wordy and hard to maintain. The implementation details can suddenly change, and all your business logic scripts will be affected.

However it's worth to note that Free monad approach has some performance issues. We definitely don't want to optimise things prematurely and probably shouldn't speak about performance without real cases and problems, but if performance is important to you, consider another approach trying to solve the same problems: `Tagless Final`. Additionally, there is many different Free monads, and they all have own properties. For example, it's possible to get some performance boost by just switching the Free type by the Church-encoded Free monad (leaving the business logic the same). In here, we won't be discussing this that much.

### Creating a Free monad language
Let's now see what steps we need to create a Free monad language. Firstly, we define the language (interface to subsystem) as GADT with constructors denoting its methods. There are several rules here that are good to follow:

* Methods should work with domain types only. No runtime or implementation data should be present on this layer.
* Methods should be as general as possible. Notice I said general not generic. General methods of this language should cover the most of use cases you can imagine, but still methods should be concrete and specific to what they mean. Going too deep into parametrical genericity and generic type level bits will harm the readability of the client code.
* It's wise to keep the language simple and responsible for a single thing (`Single Responsibility Principle`).
* Language should be consistent and minimalistic. If some action can be expressed by combination of these methods, it should not be in the language. Otherwise it will be possible to implement this behaviour differently and make a mess.

There is more rules of good language design, and it won't be a lie to say that designing functional monadic interfaces is no different than designing OOP interfaces in this sense. This is why best practices like `SOLID` are applicable in both worlds, and there is nothing bad in stealing good ideas from OOP.

In the following listing you can see the interface to Key-Value database. It's very simple.

```haskell
-- Domain types
type DBKeyRaw   = ByteString
type DBValueRaw = ByteString
 
data DBErrorType
    = SystemError
    | KeyNotFound
    | InvalidType
 
data DBError = DBError DBErrorType Text
 
type DBResult a = Either DBError a
 
-- | Interface to Key-Value database.
-- @db@ is a phantom type.
data DatabaseF db a where
    -- | Check whether the key exists.
    HasKeyRaw   :: DBKeyRaw -> (Bool -> next) -> DatabaseF db next
    -- | Lookup a value from the DB.
    GetValueRaw :: DBKeyRaw -> (DBResult DBValueRaw -> next) -> DatabaseF db next
    -- | Write a single value to the DB.
    PutValueRaw :: DBKeyRaw -> DBValueRaw -> (DBResult () -> next) -> DatabaseF db next
  deriving (Functor)
 
-- | Database language.
type DatabaseL db = Free (DatabaseF db)
 
-- | Checks whether the key exists.
hasKeyRaw :: DBKeyRaw -> DatabaseL db Bool
hasKeyRaw key = liftF $ HasKeyRaw key id
 
-- | Gets a raw value from DB by specified raw key.
getValueRaw :: DBKeyRaw -> DatabaseL db (DBResult DBValueRaw)
getValueRaw key = liftF $ GetValueRaw key id
 
-- | Writes a raw value to DB by specified raw key.
putValueRaw :: DBKeyRaw -> DBValueRaw -> DatabaseL db (DBResult ())
putValueRaw key val = liftF $ PutValueRaw key val id
```

Having a "raw" Free monad language, we can build more convenient interface over it. For example, the `DatabaseL` language has only three methods, all of them are working with raw `ByteString` data, but the framework provides additional machinery to define and work with well-typed database models. Good thing here is that we don't need to change the language itself. We just construct some additional combinators over it. This finely follows the essence of functional programming! In the following listing some code in this language is presented. Notice the code works over the typed DB language but expresses some business logic (in this case - writing entities to a database):

```haskell
-- Abstract type for data storage.
data Storage db = Storage
    { _path :: FilePath
    }
 
-- Some domain types
data KBlock = KBlock ...
data MBlock = MBlock ...
 
-- Several storages represent a database model.
data DBModel = DBModel
    { _kBlocksDB :: Storage KBlocksDB
    , _mBlocksDB :: Storage MBlocksDB
    }
 
-- Helper to call a DatabaseL script for a particular DB storage.
withKBlocksDB
    :: forall s db a
    .  Lens.HasKBlocksDB s (Storage db)
    => s
    -> DatabaseL db a
    -> NodeL a
withKBlocksDB dbModel = withDatabase (dbModel ^. kBlocksDB)
 
-- NodeL script that calls DatabaseL script.
saveKBlock :: DBModel -> KBlock -> NodeL (DBResult ())
saveKBlock dbModel kBlock =
    withKBlocksDB dbModel $ do
        putEntity' @KBlockPrevHashEntity kBlock
        putEntity' @KBlockEntity kBlock
```

It operates by some `DBModel` containing two storages: for `KBlocks` and `MBlocks`. They are just avatars, not real storages. They are abstracted from the concrete database files like `XML`, `SQLite` or whatever else, so we can safely change the underlying data representation by rewriting the interpreter. The `Storage` type holds only a path to the file. Also, it keeps a phantom type which helps to not to mix this data storage with invalid type data actions.

Ok, now we'll flip the coin of the Free monad approach and see what's on the other side. I'm talking about interpreters certainly. In the Node framework, we use `RocksDB` as implementation target for `DatabaseL`. As you can see, there is nothing about RocksDB in the language GADT, but to make actual calls we need an instance of some real DB. This is clearly a runtime bit, so we have to keep it hidden from our DB scripts. On the other hand, the `DatabaseL` interpreter can be impure, can evaluate any IO calls and it's completely fine to him to know about runtime bits like the DB type from RocksDB bindings. Take a look on it:

```haskell
writeOpts :: Rocks.WriteOptions
writeOpts = Rocks.defaultWriteOptions { Rocks.sync = True }
 
-- | Interpret DatabaseL language.
interpretDatabaseL :: Rocks.DB -> DatabaseF db a -> IO a
interpretDatabaseL db (HasKeyRaw key next) = do
    mbVal <- Rocks.get db Rocks.defaultReadOptions key
    pure $ next $ isJust mbVal
interpretDatabaseL db (GetValueRaw key next) = do
    mbVal <- Rocks.get db Rocks.defaultReadOptions key
    pure $ next $ case mbVal of
        Nothing  -> Left $ DBError KeyNotFound (show key)
        Just val -> Right val
interpretDatabaseL db (PutValueRaw key val next) = do
    r <- Rocks.put db writeOpts key val
    pure $ next $ Right r
 
-- Runner of the DatabaseL script.
runDatabaseL :: Rocks.DB -> DatabaseL db a -> IO a
runDatabaseL db = foldFree (interpretDatabaseL db)
```

So now we have three distinct layers: scripts layer, embedded domain specific language layer and impure runtime layer. Separating the code to layers helps us to control its complexity. We can reason about relevant parts only, not about the whole big picture. The code stays relatively simple and manageable.

In the Node framework, every language has its own interpreter. The interpreters are organised in the same hierarchical structure, so to run the underlying language, we need to call the underlying interpreter. Consider the following simplified example. According to the diagram above, `NodeL` scripts can evaluate `DatabaseL` scripts. The `NodeL` language can be considered a container type language, so its interpreter should reference the interpreter for the contained DatabaseL language.

We still haven't explained how `Storage` is connected to a real database, and this is where the magic comes from. We use `Storage` as a key to the real RocksDB `DB` type. This is no doubt the information for the runtime layer only and it should not be exposed to the scripts layer.

```haskell
-- | Node language.
data NodeF next where
    -- Evaluates some DatabaseL script with the corresponding storage.
    EvalDatabase :: Storage db -> DatabaseL db a -> (a -> next) -> NodeF next
 
-- Keeps reference to the concrete RocksDB DB type.
-- Also, the access to this handle should be thread safe,
-- so we use MVar as mutex here.
data DBHandle  = DBHandle
    { _db    :: Rocks.DB
    , _mutex :: MVar ()
    }
 
-- All the runtime data for the NodeL interpreter.
data NodeRuntime = NodeRuntime
    { _databases :: TVar (Map FilePath DBHandle)
    }
```

Now all we need is to interpret this properly. Here:

```haskell
-- | Runs node language. Runs interpreters for the underlying languages.
runNodeL :: NodeRuntime -> NodeL a -> IO a
runNodeL nodeRt = foldFree (interpretNodeF nodeRt)
 
-- | Interpret NodeL.
interpretNodeF :: NodeRuntime -> NodeF a -> IO a
interpretNodeF nodeRt (EvalDatabase storage dbScript next) = do
    -- Getting database map from NodeRuntime
    dbs <- readTVarIO (nodeRt ^. databases)
 
    case Map.lookup (storage ^. path) dbs of
        Nothing       -> error $ "Impossible: DB is not registered: " +|| storage ^. path ||+ "."
        Just dbHandle -> do
            -- Now we have a handle, we can run the database script.
            r <- runDatabase dbHandle dbScript
            pure $ next r
 
-- Database script runner.
-- Ensures the access to this database will be thread safe.
runDatabase :: DBHandle -> DatabaseL db a -> IO a
runDatabase dbHandle action = do
    -- Taking the mutex
    void $ takeMVar $ dbHandle ^. mutex
 
    -- Running the underlying interpreter.
    res <- runDatabaseL (dbHandle ^. db) action
 
    -- Releasing the mutex.
    putMVar (dbHandle ^. mutex) ()
    pure res
```

### Conclusion
The Node Framework gives the answer to the question we are all worrying about: is Haskell suitable for big applications? Is it possible to create a good quality code that won't become spaghetti after a while? And moreover, how to apply the approaches we have in mainstream development? I personally think we haskellers should learn from best practices that were developed during decades and should not say our language is somewhat different from the software architecture and design point of view. I know there is a big demand on topics like this one, and this is why I started writing my book "Functional Design and Architecture". Hope, this material will be useful and we will continue research in this direction.

I want to thank my team who did a significant amount of work in this project:

- Dmitry Pavluk (https://github.com/vojiranto)
- Ksenia Pozdnyakova (https://github.com/ksenia-portu)
- Daria Melnikova (https://github.com/DariaMelnikova)
- Enecuum HK Limited (https://enecuum.com)