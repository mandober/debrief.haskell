# On the Expressiveness of Purely Functional I/O Systems
Paul Hudak, Raman S. Sundaresh, 1989


Functional programming languages have traditionally lacked complete, extensible, and yet referentially transparent I/O mechanisms. Previous proposals for I/O have used either the notion of lazy streams or continuations to model interaction with the external world. We discuss and generalize these models and introduce a third, which we call the systems model, to perform I/O.

The expressiveness of the styles are compared by examples. We give a series
of simple translations between the 3 models, demonstrating that they are not
as different as their programming styles suggest, and implying that the styles could be even be mixed within a single program. The need to express non-deterministic behavior in FPL is well recognized. So is the problem of doing so without destroying referential transparency. We survey past approaches to this problem, and suggest a solution in the context of the I/O models described. The I/O system of the pFPL Haskell is presented. The system includes a rich set of operations, and distinguishes between file and channel I/O. The approach to nondeterminism is also presented. A useful aspect of the design is that it includes a rigorous specification of the behaviour of the OS, thus precisely fixing the semantics of the various I/O operations. The Haskell I/O system is capable of supporting many other paradigms of concurrent computation in a natural way. We demonstrate this through the emulation of Actors, UNITY, CSP, CCS and Linda.


## Introduction

The basic requirements for IO mechanism:
* Referential Transparency
* Efficiency
* Cooperation


## Streams

An elegant and popular model that goes a long way toward meeting the basic requirements is the use of streams, lazy lists of data objects. Several FPLs use streams for I/O, including Ponder, Hope, and Miranda. In these languages, predefined identifiers are typically provided which are bound to specific I/O channels. For example, the stream of input characters from the keyboard might have the name `kb`, and the stream of output characters to the display might have the name `display` - the OS will provide the binding for kb, and the program is expected to provide the binding for display.

Although elegant, there are at least three problems with these previous uses of the stream idiom:
- They are not completely general, since typically the I/O devices and the operations on them are pre-determined and fixed into the language.
- The semantics of interactive I/O (for example, interaction with the user) is not entirely clear (in some languages this problem is admitted up front by providing a mechanism to control the order in which the streams are consumed and produced)
- Anomalous situations are ignored; the possibility of error is generally not accounted for.

In the *stream model of I/O*, streams are used to invoke arbitrary I/O operations with arbitrary responses (typically either success or failure). In addition we retain the elegant use of streams to model interactive I/O, but we are careful to precisely define the semantics such that the input from a user, for example, can depend on output from the program (characteristic of many interactive applications). All three of the requirements stated earlier are satisfied.

## Continuations

*Continuation-based I/O model* is characterized by a set of transactions, which are functions that typically take a success continuation and failure continuation as arguments; these continuations are in turn functions that generate more transactions. The continuation model is appealing because it appears to be quite general, and the continuation structure makes it easy to reason about the sequentiality of the induced effects.

The continuation model we present here has an important exception to other implementations: we assume a non-strict (i.e. lazy) FPL, whereas previous languages using the continuation based IO have been strict. This not only simplifies the design, but in addition allows us to use the interactive lazy stream idea within the continuation model. More specifically, in PFL and Hope individual characters are read by each continuation operation, while in our model a single read operation returns a lazy stream. Thus we are able to combine the virtues of both "idioms" - streams to model demand-driven sequences of data, and continuations to enforce control restrictions.

## Stream Model

In the *stream-based I/O model*, a program is viewed as a black box that generates a stream of I/O requests; these requests are given to the OS, which performs sequaential processing and returns them to the program as a stream of responses. So, if `Request` is the datatype of requests, and `Response` is the datatype of responses, then a program `p` has type `[Response] -> [Request]`. The nth request generates the nth response. A response depends on the request and would normally include the possibility of error, which is here omitted.

Note: `resps` cannot be taken apart by pattern matching, since this would entail evaluating the response list before any requests have been issued, resulting in ⊥. In Haskell, a program has the value `main`.

```hs
main :: IO ()
main resps =
    [ AppendChannel "stdout" "type in file names\n", ReadChannel "stdin" ]
    ++ file_display (tl (tl resps))
                    (get_tokens
                        (case resp !! 2 of
                            Return user_input -> user_input))
    where
        file_display resps [] = []
        resps (name:names) = 
            [ AppendChannel "stdout" name, ReadFile name,
              AppendChannel "stdout"
                (case resps!!2 of
                    Failure msg -> "can't open file\n"
                    Return file_contents -> file_contents) ]
            ++ file_display (tl (tl (tl resps))) names
```
