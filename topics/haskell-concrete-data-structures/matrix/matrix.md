# Matrix

Matrices enjoy a range of libraries and interfaces, first and foremost BLAS (Basic Linear Algebra Subprograms) and LAPACK - Linear Algebra PACKage, which have been around since the FORTRAN days, and more recently also the GSL (GNU Scientific Library).

In Haskell, `hmatrix` pakage provides a uniform interface to much of the functionality of these 3 libraries. The functionality is split over 4 packages:
- `hmatrix`           Linear algebra
- `hmatrix-gsl`       Common numeric computations
- `hmatrix-gsl-stats` GSL statistics
- `hmatrix-special`   The "special" functions of GSL

Due to the popularity of hmatrix, there exists a whole ecosystem of packages on Hackage that either build on hmatrix, provide bindings to other standard C libraries by extending the hmatrix interface, and implement adaptors to interoperate with other array libraries (such as `hmatrix-repa` - the interop adaptor between `hmatrix` and `repa`).


## References

- http://www.netlib.org/blas/
- http://www.netlib.org/lapack/
- https://www.gnu.org/software/gsl/

* hmatrix
https://github.com/AlbertoRuiz/hmatrix
https://hackage.haskell.org/package/hmatrix
https://hackage.haskell.org/package/hmatrix-gsl
https://hackage.haskell.org/package/hmatrix-repa
https://hackage.haskell.org/package/hmatrix-special
https://hackage.haskell.org/package/hmatrix-gsl-stats
https://www.tweag.io/blog/2017-08-31-hmatrix/

* vector: Efficient Arrays
https://hackage.haskell.org/package/vector
An efficient implementation of Int-indexed arrays (both mutable and immutable), with a powerful loop optimisation framework. It includes:
- `Data.Vector` Boxed vectors of arbitrary types.
- `Data.Vector.Generic` Generic interface to the vector types.
- `Data.Vector.Storable` Unboxed vectors of Storable types.
- `Data.Vector.Unboxed` Unboxed vectors with an adaptive representation based on data type families.
- `Data.Vector.Primitive` Unboxed vectors of primitive types as defined by the primitive package. Data.Vector.Unboxed is more flexible at no performance cost.
