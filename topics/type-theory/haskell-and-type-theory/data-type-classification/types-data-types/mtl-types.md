# MTL Types

8 core notions of computation in `mtl` package:
1. identity
2. error
3. list
4. state
5. reader
6. writer
7. RWS
8. continuations


1. `IdentityT`
2. `ExceptT`  (ex `ErrorT`) exceptions based on EitherT
3. `ListT`    nondeterministic computations returning multiple results
4. `StateT`   stateful computation
5. `ReaderT`  readonly environment, fixed (readonly) function input
6. `WriterT`  appending output for logging, tracing
7. `RWST`     Reader + Writer + State combo
8. `ContT`    CPS, continuations

- `MaybeT`    fallible computation with optional uninteresting error
- `EitherT`   fallible computation with interesting error
- `IO`        IOT (?)
