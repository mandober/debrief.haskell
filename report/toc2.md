# Haskell 2010 Language Report: Contents

https://www.haskell.org/onlinereport/haskell2010/haskellli1.htm

## II The Haskell 2010 Libraries

13 Control.Monad
 13.1 Functor and monad classes
 13.2 Functions
  13.2.1 Naming conventions
  13.2.2 Basic Monad functions
  13.2.3 Generalisations of list functions
  13.2.4 Conditional execution of monadic expressions
  13.2.5 Monadic lifting operators
14 Data.Array
 14.1 Immutable non-strict arrays
 14.2 Array construction
 14.3 Accessing arrays
 14.4 Incremental array updates
 14.5 Derived arrays
 14.6 Specification
15 Data.Bits
16 Data.Char
 16.1 Characters and strings
 16.2 Character classification
  16.2.1 Subranges
  16.2.2 Unicode general categories
 16.3 Case conversion
 16.4 Single digit characters
 16.5 Numeric representations
 16.6 String representations
17 Data.Complex
 17.1 Rectangular form
 17.2 Polar form
 17.3 Conjugate
 17.4 Specification
18 Data.Int
 18.1 Signed integer types
19 Data.Ix
 19.1 The Ix class
 19.2 Deriving Instances of Ix
20 Data.List
 20.1 Basic functions
 20.2 List transformations
 20.3 Reducing lists (folds)
  20.3.1 Special folds
 20.4 Building lists
  20.4.1 Scans
  20.4.2 Accumulating maps
  20.4.3 Infinite lists
  20.4.4 Unfolding
 20.5 Sublists
  20.5.1 Extracting sublists
  20.5.2 Predicates
 20.6 Searching lists
  20.6.1 Searching by equality
  20.6.2 Searching with a predicate
 20.7 Indexing lists
 20.8 Zipping and unzipping lists
 20.9 Special lists
  20.9.1 Functions on strings
  20.9.2 ”Set” operations
  20.9.3 Ordered lists
 20.10 Generalized functions
  20.10.1 The ”By” operations
   20.10.1.1 User-supplied equality (replacing an Eq context)
   20.10.1.2 User-supplied comparison (replacing an Ord context)
  20.10.2 The ”generic” operations
21 Data.Maybe
 21.1 The Maybe type and operations
 21.2 Specification
22 Data.Ratio
 22.1 Specification
23 Data.Word
 23.1 Unsigned integral types
24 Foreign
25 Foreign.C
26 Foreign.C.Error
 26.1 Haskell representations of errno values
  26.1.1 Common errno symbols
  26.1.2 Errno functions
  26.1.3 Guards for IO operations that may fail
27 Foreign.C.String
 27.1 C strings
  27.1.1 Using a locale-dependent encoding
  27.1.2 Using 8-bit characters
 27.2 C wide strings
28 Foreign.C.Types
 28.1 Representations of C types
  28.1.1 Integral types
  28.1.2 Numeric types
  28.1.3 Floating types
  28.1.4 Other types
29 Foreign.ForeignPtr
 29.1 Finalised data pointers
  29.1.1 Basic operations
  29.1.2 Low-level operations
  29.1.3 Allocating managed memory
30 Foreign.Marshal
31 Foreign.Marshal.Alloc
 31.1 Memory allocation
  31.1.1 Local allocation
  31.1.2 Dynamic allocation
32 Foreign.Marshal.Array
 32.1 Marshalling arrays
  32.1.1 Allocation
  32.1.2 Marshalling
  32.1.3 Combined allocation and marshalling
  32.1.4 Copying
  32.1.5 Finding the length
  32.1.6 Indexing
33 Foreign.Marshal.Error
34 Foreign.Marshal.Utils
 34.1 General marshalling utilities
  34.1.1 Combined allocation and marshalling
  34.1.2 Marshalling of Boolean values (non-zero corresponds to True)
  34.1.3 Marshalling of Maybe values
  34.1.4 Marshalling lists of storable objects
  34.1.5 Haskellish interface to memcpy and memmove
35 Foreign.Ptr
 35.1 Data pointers
 35.2 Function pointers
 35.3 Integral types with lossless conversion to and from pointers
36 Foreign.StablePtr
 36.1 Stable references to Haskell values
  36.1.1 The C-side interface
37 Foreign.Storable
38 Numeric
 38.1 Showing
 38.2 Reading
 38.3 Miscellaneous
39 System.Environment
40 System.Exit
41 System.IO
 41.1 The IO monad
 41.2 Files and handles
  41.2.1 Standard handles
 41.3 Opening and closing files
  41.3.1 Opening files
  41.3.2 Closing files
  41.3.3 Special cases
  41.3.4 File locking
 41.4 Operations on handles
  41.4.1 Determining and changing the size of a file
  41.4.2 Detecting the end of input
  41.4.3 Buffering operations
  41.4.4 Repositioning handles
  41.4.5 Handle properties
  41.4.6 Terminal operations
  41.4.7 Showing handle state
 41.5 Text input and output
  41.5.1 Text input
  41.5.2 Text output
  41.5.3 Special cases for standard input and output
42 System.IO.Error
 42.1 I/O errors
  42.1.1 Classifying I/O errors
  42.1.2 Attributes of I/O errors
 42.2 Types of I/O error
 42.3 Throwing and catching I/O errors
