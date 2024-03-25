# Testing

The Design and Use of QuickCheck
https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html

QuickCheck manual
http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html

smallcheck: test your Haskell code by exhaustively checking its properties
https://github.com/feuerbach/smallcheck

stack code coverage
https://docs.haskellstack.org/en/latest/coverage/



# Testing with QuickCheck

QC is a property-based testing tool.

A *property* of a program is an observation that we expect to hold true regardless of the program's inputs. Invariants.

It may involve only the output ("always outputs a positive number") or compare input and output ("preserves list length") or even assess external effects ("matches the output of a trusted external program").

Instead of writing individual unit tests (consisting of actual/expected input/output pairs of a function), we write the desired properties of functions. Then QuickCheck automatically generates random values which can be run to verify (or rather, falsify) the property.

QuickCheck finds the edge cases that violate the specs, which leads to either fixing the code or the specs (we either discover incomplete specs or bugs). Additionally, the specs persist as rich machine-checkable documentation.

QuickCheck can be seen as a formal methods that is quick to indicate the difference between program's *specification* and its *implementation*. The inconsistencies between them reveal bugs and/or the lack of understanding of our program's specification.

Programming is not a one-way street from specifications to implementation, but it reacts and evolves according to the feedback loop between the two. Property-based testing accelarates this evolution.

The only way to really make sure the properties of a program always hold is by doing a mathematical proof. Property-based testing cannot reach that level, but it can produce reasonably good results with far less effort invested. It does so by checking properties against a finite number of randomized inputs, called *test cases*.


## Methodology of Testing

Property based testing can discover bugs, but it cannot guarantee the total absence of bugs.

With that in mind, what requirements must be met to consider QuickCheck tests adequate? One requirement is the complete *code coverage*, which implies that sufficient number of test cases is generated such that every path in a program is traversed at least once.

QuickCheck does not measure code coverage, nor does it deliberately generate tests that exercise particular code paths. Therefore, a code coverage tool makes a good companion to QuickCheck.

For starters, Stack's got your back: `$ stack test --coverage`. See more at:   
https://docs.haskellstack.org/en/latest/coverage/

Random data tends to exercise small programs considerably, so applying QuickCheck against one small function at a time is a good way to build total coverage. Test cases whose statistical distribution follows that of actual data tend to generate higher coverage.

For example, when testing the validity of an email address, the majority of random strings will end up triggering the else/failure code path. QuickCheck's built-in test case generators use a uniform distribution, but the library provides a way to customize generation to make it more realistic.

There are situations when random test cases are not the most effective tool. QuickCheck will be less definitive than an alternative like **SmallCheck** for functions which take a finite number of possible input values. SmallCheck generates not just a random set of test cases but all possible cases beneath a given "size". For example, all Unicode strings of length 10 or less. If a function has a finite (and small enough) number of possible input values then SmallCheck can prove *conclusively* when properties hold.

## Generating Random Test Cases

QuickCheck generates random test cases in order to try and falsify program properties.

It uses a newtype with a single type argument, `Gen a`, to generate values of type `a`. `Gen` wraps a function mapping a random number generator and a size parameter to a value of a given type.

```hs
newtype Gen a = MkGen {
  unGen :: QCGen -> Int -> a
}
```

QuickCheck provides combinators to obtain and modify Gen values. 
For instance, here are two basic functions to make Gens:

```hs
-- Generate a random element in the given inclusive range
-- using Random from the "random" package
choose :: Random a => (a, a) -> Gen a

-- Pick elements at random from a (non-empty) list
elements :: [a] -> Gen a
```

You get values out of Gen by using `generate`. It runs in the IO monad so that it can advance along an internal sequence of pseudo-random values.

```hs
-- generate :: Gen a -> IO a

-- produce 1, 2, or 3
generate $ elements [1,2,3]

-- produce a lowercase letter
generate $ choose ('a', 'z')

-- produce a constant value (since Gen has a Monad instance)
generate $ return 1
```
