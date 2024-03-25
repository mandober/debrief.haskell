# State

`get` is a primitive function of the State monad, that exposes the hidden state. The actual piece of state in the State moand is hidden and threaded around by the bind function. To get that state we use the `get` function.
