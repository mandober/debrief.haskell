# Array

The proper C-like array is a stack data structure, the most efficient of them all because it is an implicit data structure (it has no administrative overhead) and because it enjoys a great locality of reference (memory addresses near the currenty accessed location are most likely to be accessed in the near future, and so they are aggressively cached).

## Array programming in Haskell

The simplest way to approach array programming is by relying on a set of standardized array routines with well-understood semantics, performance characteristics and applications, such as *BLAS* (Basic Linear Algebra Subprograms) and *LAPACK* (Linear Algebra Package). In the Haskell ecosystem, this functionality is provided by the `hmatrix` (Numeric Linear Algebra), which is simply a wrapper around the BLAS and LAPACK C libraries. It also has a few companion libraries to support the additional functionality found in the *GNU Scientific Library* and the *GNU Linear Programming Kit*. In addition to the CPU libraries, `cublas` package provides access to GPU-accelerated variants of these standard array routines.

## Standard arrays

When the standard routines are not sufficient for the problem at hand, we can fall back to a range of libraries that provide basic array building blocks that we can use to implement our own array algorithms. This is where the choice starts to get overwhelming.

There are immutable lazy Haskell 2010 arrays. On top of that, the `array` package bundled with GHC provides a range of mutable and immutable arrays for use with boxed and unboxed data, including support for low-level, C-style, hand-tuned imperative array code. There are a few more packages that provide similar functionality, but differ in how arrays are stored and how memory is managed.

## Single-core

Beyond array, we can categorise different packages by whether they are designed to support parallelism by simultaneously executing code on multiple CPU or GPU cores. The most popular choice for fast single-core arrays is the package `vector` (Efficient Arrays) - one of the spin-offs of the *Data Parallel Haskell* project. The package `vector` produces not only highly efficient code using a range of *array fusion techniques*, it also supports a wide range of array flavours, including the storage of boxed and unboxed data. Due to its popularity, vector has grown its own ecosystem of interoperability and extension packages, including access to highly optimised *Fourier transforms* based on *FFTW*.

The fundamental difference between `array` and `vector` is that array provides a mostly index-based interface to the programmer, which allows for great control, but also imposes an imperative style of programming. In contrast, vector favours whole-vector processing collective operations (also referred to as wholemeal programming). This raises the level of abstractions, but it also puts a larger burden on vector and the Haskell compiler to compile this high-level code to efficient machine code.

* vector
https://hackage.haskell.org/package/vector

* Data Parallel Haskell
https://wiki.haskell.org/GHC/Data_Parallel_Haskell

* Fastest Fourier Transform in the West
https://en.wikipedia.org/wiki/FFTW

* La Tour De Hanoi (Functional Pearl) Ralf Hinze 2009
http://www.cs.ox.ac.uk/ralf.hinze/publications/ICFP09.pdf
Example of wholemeal programming


## Multicore

If we want parallelism, the choice is mostly between repa: High performance, regular, shape polymorphic parallel arrays (another spin-off from Data Parallel Haskell) and Accelerate: High-Performance Haskell (the successor to Data Parallel Haskell). Both libraries have many similarities, for example, in the support for shape-polymorphic array code, but also exhibit a fundamental difference: Repa is a standard collection-oriented Haskell library , much like vector, whereas Accelerate is an embedded array language in Haskell.

The major technical consequence out of that distinction is that Repa code is compiled prior to runtime with GHC's standard code generator. In contrast, Accelerate code is just-in-time compiled at application runtime using a custom code generator. In fact, there are multiple code generators, which results in support for multicore CPUs as well as GPUs. This added flexibility and, in many cases, performance advantage comes at the expense of a somewhat less expressive and more involved programming model.

* High-Performance Parallel Arrays for Haskell
http://www.acceleratehs.org/

* https://hackage.haskell.org/package/repa

* https://wiki.haskell.org/GHC/Data_Parallel_Haskell



## Beyond

In addition to the packages explicitly mentioned above, there are many experimental, incomplete, or unsupported alternatives. Some of them incorporate great ideas and some show impressive performance. However, the criteria for the packages explicitly mentioned in this post are that a package must (1) be available on Hackage, (2) be actively maintained, and (3) be used outside of the group that develops it.

Let me know what your favourite array library is and what you are using it for. In the next instalment, we will have a closer look at hmatrix.





## References

* Array programming in Haskell
https://www.tweag.io/blog/2017-08-09-array-programming-in-haskell/

* Immutable lazy Haskell 2010 arrays
https://www.haskell.org/onlinereport/haskell2010/haskellch14.html#x22-20100014

* hmatrix
https://hackage.haskell.org/package/hmatrix
http://dis.um.es/~alberto/hmatrix/hmatrix.html
https://www.tweag.io/blog/2017-08-31-hmatrix/

* cublas
https://hackage.haskell.org/package/cublas

* BLAS - Basic Linear Algebra Subprograms
https://en.wikipedia.org/wiki/Basic_Linear_Algebra_Subprograms

* LAPACK - Linear Algebra Package
https://en.wikipedia.org/wiki/LAPACK

* GNU Scientific Library
https://en.wikipedia.org/wiki/GNU_Scientific_Library

* GNU Linear Programming Kit
https://en.wikipedia.org/wiki/GNU_Linear_Programming_Kit
