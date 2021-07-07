# Streams: TERMS

Paradigms
  stream processing
    event stream processing
  reactive programming
    functional reactive programming
  dataflow programming

Tech
  big data
  map-reduce

Input/Output
  Lazy IO
  Handler IO, handler-based I/O
  Iteratee IO, iteratee-based I/O

Stream
  stream (sequence of data)
  composable stream components
  intermediate streams
  stream fusion
  uniform streaming

Entities
  iterator
  iteratee
  enumerator
  enumeratee
  generator, generating function

Pipelines
  data pipeline
  transformer
  kernel function (series of operations)
  data operator
  data transformer (map, filter)
  data reducer, reducer
  data producer, producer
  data sink
  data source
  data consumer, consumer




## Reactive Programming
  Reactive Programming theory
  propagation of change
  data streams
    static data streams (arrays)
    dynamic data streams (event emitters)
  automatic propagation
  propagation of updates
  data flow
  * RP semantics
  - synchrony (a/sync underlying model of time)
  - determinism (non/deterministic in evaluation and results)
  - update process (callbacks, dataflow, actor)
  * Change propagation algorithms
    - relations between value consumers value producers
    * approaches to data propagation:
      - pull (polling)
      - push
      - push-pull
    - event reaction
    - delta propagation
    - incremental change propagation
    - change accumulation
    - batch propagation
    - invalidity notification propagation
    - event loop
    - callbacks
    - control inversion
    * glitches
    * dependency graph
      - DAGs
      - cyclic dependencies
    * mutability
      - mutable operations
      - mutable cell
    * RP concepts
      - Degrees of explicitness
      - Static or dynamic
      - Higher-order reactive programming
      - Data flow differentiation
      - Evaluation models of reactive programming
        - invalidate/lazy-revalidate scheme
        - Similarities with observer pattern
    * RP Approaches
      - Imperative
      - OORP
      - FRP
      - Actor based
      - Rule based
