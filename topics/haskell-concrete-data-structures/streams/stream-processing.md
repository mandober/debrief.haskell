# Stream processing

https://en.wikipedia.org/wiki/Stream_processing

Stream processing is a computer programming paradigm, equivalent to *dataflow programming*, *event stream processing* and *reactive programming*, that allows some applications to more easily exploit a limited form of parallel processing.

Such applications can use multiple computational units, such as the FPU on a GPU or FPGAs without explicitly managing allocation, synchronization or communication among those units.

The stream processing paradigm simplifies parallel software and hardware by restricting the parallel computation that can be performed.

Given a sequence of data, i.e. a *stream*, and 
a series of operations, i.e. *kernel functions*, 
kernel functions are applied to each element in the stream.

Kernel functions are usually pipelined, and optimal local on-chip memory reuse is attempted, in order to minimize the loss in bandwidth, associated with external memory interaction.

*Uniform streaming*, where one kernel function is applied to all elements in the stream, is typical. Since the kernel and stream abstractions expose data dependencies, compiler tools can fully automate and optimize on-chip management tasks.

## Applications

Stream processing is essentially a compromise, driven by a data-centric model that works very well for traditional DSP or GPU type applications (such as signal processing) but less so for general purpose processing with more randomized data access. By sacrificing some flexibility in the model, the implications allow easier, faster and more efficient execution. Depending on the context, processor design may be tuned for maximum efficiency or a trade-off for flexibility.

Stream processing is especially suitable for uses that exhibit these 3 features:

1. *Compute(ation) intensity*: the number of arithmetic operations per I/O or global memory reference. In many signal processing applications today, it is well over 50:1 and increasing with algorithmic complexity.

2. *Data parallelism* exists in a kernel if the same function is applied to all records of an input stream and a number of records can be processed simultaneously without waiting for previous results.

3. *Data locality* is a specific type of temporal locality common in signal processing applications where data is produced once, read once or twice later in the application, and never looked again. Intermediate streams passed between kernels, as well as intermediate data within kernel functions, can capture this locality directly using the stream processing programming model.

Examples of records within streams include:
- In graphics, each record may be a vertex, normal, color info for a triangle
- In image processing, each record may be a single pixel from an image
- In a video encoder, each record may be 256 pixels forming a data macroblock
- In wireless signal processing, each record may be a sequence of samples received from an antenna.

For each record we can only read from the input, perform operations on it, and write to the output. It is permissible to have multiple inputs and multiple outputs, but never a piece of memory that is both readable and writable.
