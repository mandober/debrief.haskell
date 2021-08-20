# Demystifying Type Classes

> Describing three strategies of implementing  type classes, aiming to clarify their behavior

The most well-known technique of implementing type classes is so-called dictionary passing. Although historically the second, it is the first to study as it helps understand the others. We explain the dictionary passing by a progression of examples that cover the most common patterns of Haskell98-like type classes, instances and polymorphic functions: from simple `Show` to polymorphic recursion. The examples are inspired by the numeric hierarchy of Haskell. They contrast Haskell98 code with its \`translation' into OCaml, a sample higher-order language without type classes. Our simple subset of OCaml may be regarded as a friendlier dialect of GHC Core, an intermediate type-class--free language of GHC. This translation exposes the compilation strategy, explaining what happens with type classes as GHC translates the source code to Core. We can use the translation to bring type classes to any functional language -- although the lack of the syntactic sugar bestowed by the type class abstraction is jarring.

We start with the simplest overloaded function to show values of various types as a string: the `Show` type class. (The Haskell standard Prelude class `Show` is a bit more complex and optimized than ours; the main idea is the same.) Below is the declaration of our simple `Show` and two of its instances -- and the corresponding OCaml code. We always write type signatures, even though they are inferred, except for the very last example of polymorphic recursion.

Haskell

    class Show a where
      show :: a -> String
    
    instance Show Bool where
      show True  = "True"
      show False = "False"
    
    instance Show Int where
      show x = Prelude.show x  -- internal

OCaml

    type 'a show = {show: 'a -> string}
    
    let show\_bool : bool show = 
      {show = function
        | true  -> "True"
        | false -> "False"}
    
    let show\_int : int show =
      {show = string\_of\_int}

The type-class declaration `Show a` in Haskell translates to the data type declaration for the record `'a show`: the dictionary. The type-class method name becomes the label in the dictionary.

As an example of using the just introduced `show`, we define an overloaded function to print values of various types, and instantiate it to print a boolean.

Haskell

    print :: Show a => a -> IO ()
    print x = putStrLn $ show x
    
    test\_print :: IO ()
    test\_print = print True

OCaml

    let print : 'a show -> 'a -> unit =
      fun {show=show} x -> print\_endline (show x)
    
    let test\_print : unit =
      print show\_bool true

Let's compare the inferred types of the `print` function, in Haskell and OCaml. They are, respectively:

    print :: Show a =>  a -> IO ()
    print : 'a show -> 'a -> unit

Both in Haskell and OCaml, `print` is a polymorphic function. However, its polymorphism is restricted, or bounded: `print` applies to values of only those types that are showable, that is, are members of the `Show` class -- and we have the evidence for their membership. This is (painfully) explicit in OCaml's `print`: the evidence of being showable, the dictionary, is the explicit argument. It is the programmer's responsibility to find this evidence and pass it to `print`: see `test_print`'s code. In Haskell, the polymorphism restriction takes the form of a so-called type-class constraint `Show a`. It, too, may be regarded as the argument carrying the membership evidence. However, it is the Haskell compiler that finds and passes the evidence. The `Show a` argument is therefore implicit, which is indicated by the double-arrow in `print`'s type.

The OCaml type of `print` reveals the nature of bounded polymorphism. An unbounded polymorphic function such as `id : 'a -> 'a` corresponds to the universally quantified proposition `forall a. a -> a`. The function `print` witnesses the proposition `a -> unit` quantified only over a part of the domain of discourse. The predicate `show` decides the membership in that part: we assert `a -> unit` only when `show(a)`. The proposition thus reads `forall a. show(a) -> (a -> unit)` -- which is the type of `print` modulo stylistic differences.

Next is the (simplified) `Num` type class, whose methods have a different pattern of overloading: the method `fromInt` is overloaded on the result type, and the method `(+)` is binary.

Haskell

    class Num a where
      fromInt :: Int -> a
      (+)     :: a -> a -> a
    
    sum :: Num a => \[a\] -> a  -- sample function
    sum ls = foldr (+) (fromInt 0) ls

OCaml

    type 'a num = 
        {fromInt: int -> 'a;
         add:     'a -> 'a -> 'a}
    
    let sum : 'a num -> 'a list -> 'a = 
      fun {fromInt = fromInt; add = add} ->
      fun ls -> List.fold\_right add ls (fromInt 0)

The `Num` type class has two methods; therefore, the corresponding dictionary record `num` has two fields. The instances for `Bool` and `Int`, and their translations, are straightforward. We show only the `Int` instance:

Haskell

    instance Num Int where
      fromInt x = x
      (+)       = (Prelude.+)

OCaml

    let num\_int : int num = 
      {fromInt = (fun x -> x);
       add     = Stdlib.(+)}

The polymorphic function `print_incr`, below, to print an incremented value, depends on two constraints:

Haskell

    print\_incr :: (Show a, Num a) => a -> IO ()
    print\_incr x = print $ x + fromInt 1
    
    print\_incr\_int :: Int -> IO () -- A sample instantiation
    print\_incr\_int x = print\_incr x

OCaml

    let print\_incr : ('a show \* 'a num) -> 'a -> unit =
      fun (show\_dict, {fromInt=fromInt;add=(+)}) ->
      fun x -> print show\_dict (x + fromInt 1)
    
    let print\_incr\_int : int -> unit = fun x ->
      print\_incr (show\_int,num\_int) x

The inferred type of Haskell's `print_incr` has the pair of constraints `(Show a,Num a)`. In the translation, it becomes the pair of dictionaries, `'a show` and `'a num`, passed to `print_incr` as the explicit argument. OCaml's `print_incr` needs `show_dict` to pass it to the parametrically overloaded `print` (talked about earlier). The numeric operations extracted from `num_dict` are used when incrementing. All this boilerplate of dictionary passing and extraction is explicit in OCaml, but implicit in Haskell. A Haskell compiler handles this boilerplate for us.

To instantiate a bound-polymorphic function in Haskell we merely have to use it in a specific type context or give a specific type, see Haskell's `print_incr_int`. The type checker will verify that the specific type, `Int` in our case, is the member of `Show` and `Num`. These constraints of `print_incr` become resolved and no longer appear in the type of `print_incr_int`. On the OCaml side, we don't just make the type variable `'a` to be `int` and let the type checker verify the constraint satisfaction. It is the programmer who has to prove that the constraints are indeed satisfied: the programmer has to find and explicitly pass the dictionaries `show_int` and `num_int`, as the proof that `int` is indeed a member of `Show` and `Num`. The type class abstraction does such proofs for us, searching for dictionaries and combining them in the complete evidence to pass to a parametrically overloaded function.

The next common pattern is an instance with a constraint: a `Show` instance for all list types `[a]` where the element type `a` is also restricted to be a member of `Show`.

Haskell

    instance Show a => Show \[a\] where
      show xs = "\[" ++ go True xs
       where
         go \_ \[\]    = "\]"
         go first (h:t) = (if first then "" else ", ") ++ show h ++ go False t
    
    testls :: String
    testls = show \[1::Int,2,3\]

OCaml

    let show\_list : 'a show -> 'a list show = fun {show=show} ->
      {show = fun xs ->
       let rec go first = function
         | \[\]   -> "\]"
         | h::t -> (if first then "" else ", ") ^ show h ^ go false t
       in "\[" ^ go true xs}
    
    let testls : string = 
      (show\_list show\_int).show \[1;2;3\]

The `instance Show a => Show [a]` now translates to a function, which receives the `'a show` dictionary, the evidence that `'a` is a member of `Show`, and produces the evidence that `'a list` is also a member. As before `` `=>'`` becomes `` `->'`` in the translation. The occurrence `show h` in the Haskell code is not a recursive reference to the list show being defined. Rather, it refers to the `show` at a different type, the type of list elements. The OCaml code makes this reference clear. The specialization, `testls`, again involves more work on the OCaml side: we have to build the proof that `int list` is showable, by finding the evidence that `int` is showable and passing it to `show_list` to obtain the desired proof, that is, the function for showing integer lists.

For the final examples we need a class of comparable types:

    class Eq a where
      (==) :: a -> a -> Bool

Its `Bool` and `Int` instances, and the corresponding dictionary `type 'a eq = {eq: 'a -> 'a -> bool}` are straightforward and elided. More interesting is the type class with a super-class and a default method:

Haskell

    class (Eq a, Num a) => Mul a where
      (\*) :: a -> a -> a
      x \* \_ | x == fromInt 0 = fromInt 0
      x \* y | x == fromInt 1 = y
      x \* y = y + (x + (fromInt (-1))) \* y
    
    instance Mul Bool where
      -- default
    
    instance Mul Int where
      x \* y = (Prelude.\*) x y  -- internal

OCaml

    type 'a mul = {mul\_super: 'a eq \* 'a num;
                   mul: 'a -> 'a -> 'a}
    
    let mul\_default : 'a eq \* 'a num -> 'a mul =
      fun (({eq=eq},{fromInt=fromInt;add=(+)}) as super) ->
      {mul\_super = super;
       mul = let rec loop x y = match () with
      | () when eq x (fromInt 0) -> fromInt 0
      | () when eq x (fromInt 1) -> y
      | () -> y + loop (x + (fromInt (-1))) y
      in loop}
    
    let mul\_bool : bool mul = mul\_default (eq\_bool,num\_bool)
    
    let mul\_int : int mul =
      {mul\_super=(eq\_int,num\_int); mul=Stdlib.( \* )}

The default code for the multiplication recursively refers to the multiplication being defined. This reference happens at the same type: the recursion is ordinary, not polymorphic. Again this is apparent in the translation.

The constraint `(Eq a, Num a)` in the class `Mul` declaration has a subtlety, revealed in the OCaml translation. Recall the earlier `instance Show a => Show [a]`. One may read it as a conditional (qualified) declaration: the type `[a]` is a member of the `Show` class provided `a` is a member. The OCaml translation made the qualification clear: `show_list` is a function that takes an `'a show` dictionary (the evidence of the `Show a` membership) and returns `'a list show`: the evidence that the list type is also showable. One may be tempted to regard `class (Eq a, Num a) => Mul a` similarly, as a qualified declaration. It is not -- and the OCaml translation makes it clear. We see that `mul_bool` and `mul_int` are not functions: they are dictionaries, which include the pair of dictionaries `'a eq` and `'a num`. The two are hence _provided_ by the `'a mul` dictionary.

The fact that `Mul a` provides, rather than requires, the `Num a` and `Eq a` membership evidence is clear from the type of a sample function: computing dot-product.

Haskell

    dot :: Mul a => \[a\] -> \[a\] -> a
    dot xs ys = sum $ zipWith (\*) xs ys
    
    test\_dot :: Int
    test\_dot = dot \[1,2,3\] \[4,5,6\]

OCaml

    let dot : 'a mul -> 'a list -> 'a list -> 'a =
      fun {mul\_super=(eq,num);mul=mul} ->
      fun xs ys -> sum num @@ List.map2 mul xs ys
    
    let test\_dot : int = 
      dot mul\_int \[1;2;3\] \[4;5;6\]

The OCaml translation `dot` receives only `'a mul` but does not only multiplication but also addition. One may feel that the constraint in the class declaration should have been written as `class (Eq a, Num a) <= Mul a`. In fact, such a syntax has been suggested. The different interpretation of constraints in instance and class declarations is known, but not well, and can be confusing.

The final example deals with polymorphic recursion. Type signatures become mandatory.

Haskell

    print\_nested :: Show a => Int -> a -> IO ()
    print\_nested 0 x = print x
    print\_nested n x = print\_nested (n-1) (replicate n x)
    
    test\_nested = do
      n <- getLine
      print\_nested (read n) (5::Int)

OCaml

    let rec print\_nested : 'a. 'a show -> int -> 'a -> unit =
      fun show\_dict -> function 
        | 0 -> fun x -> print show\_dict x
        | n -> fun x -> print\_nested (show\_list show\_dict) (n-1) (replicate n x)
    
    let test\_nested =
      let n = read\_int () in
      print\_nested show\_int n 5

At first blush, the code is straightforward. After seeing the output one realizes that the type of the printed value, the deeply nested list `[[...[Int]...]]`, is not statically known. It depends on the value of `n` received from the user at run-time. Since we do not know the exact type of `x` at compile time, we cannot statically build the evidence that it is showable. The compiler must arrange for building such evidence dynamically. The OCaml code illustrates such an arrangement: as we add one more `list` to the type, we transform the current `show_dict` with one more `show_list`.

The explicit construction, deconstruction and passing of dictionaries in the OCaml code is annoying. What makes type classes popular in Haskell is the hiding of all this plumbing. The convenience increases when two type classes are involved, e.g., `(Show a, Num a)` in `print_incr`. In Haskell `(Show a, Num a)` and `(Num a, Show a)` are the same constraints -- but the corresponding types in OCaml `('a show * 'a num)` and `('a num * 'a show)` are different. Actually, OCaml has extensible records, in which the order of fields does not matter. These records are more appropriate for modeling dictionaries.

In conclusion, we have described the dictionary passing implementation of type classes by the way of a translation to OCaml, a sample higher-order language. The double-arrow is translated to the ordinary arrow: the type class constraint becomes the explicit dictionary argument, the evidence of the constraint satisfaction. Therefore, in OCaml we have to explicitly pass the dictionary argument to all bounded polymorphic functions. In Haskell, the dictionary is an implicit argument and the Haskell compiler does a great job of filling it in where needed, hiding the argument from the user. Overloading over type constructor (e.g., `Monad` class) is conceptually similar, but requires type constructor polymorphism in the language. Haskell constructor classes hence need OCaml functors. Conversely, OCaml and SML modules (including sealing, generative and applicative functors and recursive structures) can be emulated as Haskell constructor type classes, see the bibliography at the end.

We now look at the other two implementations of type classes and contrast them with the dictionary passing implementation, using the examples from the present section.


[Source](http://okmij.org/ftp/Computation/typeclass.html)