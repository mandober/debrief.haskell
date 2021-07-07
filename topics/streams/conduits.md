# Conduits

- the `conduit` package is one of the solutions for lazy IO troubles

## Conduits
(What I Wish I Knew When Learning Haskell)


## Conduit manual
https://github.com/snoyberg/conduit#readme

Conduit is a framework for dealing with streaming data, such as reading raw bytes from a file, parsing a CSV response body from an HTTP request, or performing an action on all files in a directory tree. It standardizes various interfaces for streams of data, and allows a consistent interface for transforming, manipulating, and consuming that data.

Some of the reasons you'd like to use conduit are:
- Constant memory usage over large data
- Deterministic resource usage (e.g., promptly close file handles)
- Easily combine different data sources (HTTP, files) with data consumers (XML/CSV processors)
- Want more motivation on why to use conduit? Check out this presentation on conduit. Feel free to ignore the yesod section.

## Core type

```hs
type Conduit :: * -> (* -> *) -> * -> *
type Conduit i m o = ConduitT i o m ()

type ConduitT :: * -> * -> (* -> *) -> * -> *
newtype ConduitT i o m r =
        ConduitT { unConduitT :: forall b.
            (r -> Pipe i i o () m b)
               -> Pipe i i o () m b }

newtype ConduitT i o m r = ...
```

Core datatype of the conduit package. 
The `ConduitT` newtype represents a general component which can:
- consume a stream of input values `i`
- produce a stream of output values `o`
- perform actions in the `m` monad
- produce a final result `r`
