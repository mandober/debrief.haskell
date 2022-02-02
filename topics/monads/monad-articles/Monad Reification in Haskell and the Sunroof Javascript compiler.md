---
downloaded:       2022-01-01
page-url:         https://web.archive.org/web/20121105051310/http://www.ittc.ku.edu/csdlblog/?p=88
page-title:       Monad Reification in Haskell and the Sunroof Javascript compiler « CSDL Blog
article-title:    Monad Reification in Haskell and the Sunroof Javascript compiler « CSDL Blog
---
# Monad Reification in Haskell and the Sunroof Javascript compiler « CSDL Blog

It is possible to reify a monad in Haskell. This took us by surprise; we did not think this was possible. Here is the story of how reification on a monad works.
It is possible to reify a monad in Haskell. This took us by surprise; we did not think this was possible. Here is the story of how reification on a monad works.

## Reification

Reification is the observation of underlying structure. One reason for reification is to cross-compile a Haskell expression to execute on a different platform. This has been done many times, and a great reference for this is Conal Elliott, Sigbjorn Finne and Oege de Moor’s [Compiling Embedded Languages][1]

The trick is to build a structure when a direct interpretation would be typically used. A canonical example is arithmetic.

```
data Expr where
  Lit :: Int          -> Expr
  Add :: Expr -> Expr -> Expr

instance Num Expr where
  e1 + e2 = Add e1 e2
  fromInteger i = Lit (fromInteger i)
```

Now we do things like

```
GHCi> 1 + 2
Add (Lit 1) (Lit 2)
```

and get the *structure* of the computation of the expression, not just the value. This is called a deeply embedded language; the structure is deeply embedded inside the expression.

From here we can use this structure to, among other things, compile and execute this expression in a different setting. In “Compiling Embedded Languages”, the target was executing C graphics code.

## Reification of Functions

It is possible to reify a function. The trick here is to provide a prototypical version of the input argument, and observe how it used in the result.

```
data Expr where
  -- new constructor
  Var :: String       -> Expr
  -- as before
  Lit :: Int          -> Expr
  Add :: Expr -> Expr -> Expr

reifyFn :: (Expr -> Expr) -> Expr
reifyFn fn = fn (Var "x")
```

Now we can reify the function.

```
> let f x = x + 2 :: Expr
> reifyFn f
Add (Var "x") (Lit 2)
```

We use this trick extensively in [Kansas Lava][2], and other DSLs.

## Reification of Monads

Providing a deep embedding of a Monad is straightforward. Consider:

```
data M :: * -> * where
   Return :: a -> M a
   Bind :: M a -> (a -> M b) -> M b
   GetChar :: M Char
   PutChar :: Char -> M ()
```

The issue is how do we reify `Bind`? Or more specifically, how do we provide the prototypical variable of type ‘a’, to reify the 2nd argument of `Bind`? For a long time, I assumed this was not possible, without a post-hoc constraint on Bind for a type that provided the appropriate `Var ".."`.

However, there is a way of doing this, by normalizing structure of the monad. This trick was introduced by Chuan-kai Lin in [Unimo][3] and is used by the Heinrich Apfelmus in his [`operational`][4] hackage package. We work with operational, because (1) Heinrich Apfelmus contacted us, pointing out that his library could simplify our ad-hoc unrolling mechanism, and (2) it was available on hackage.

`operational` uses the left identity and associativity monad rules to normalize a monadic program into a stream of primitive instructions terminated by a `return`.

```
Program ::= Primitive >>= Program
         |  return a
```

Using `operational` is easy, you define the primitive(s), and then you can `view` your program.

```
import Control.Monad.Operational

data MI :: * -> * where
       GetChar :: MI Char
       PutChar :: Char -> MI ()

-- bind and return provided by operational
type M a = Program MI a

compile :: M a -> ...
compile = eval . view
   where
     eval :: ProgramView MI a -> ...
     eval (PutChar ch :>>= g) = ...
     eval (GetChar :>>= g) = ...
     eval (Return b) = ..
```

This effectively gives you a deep embedding. Neat stuff.

## Sunroof: The Javascript Monad

Javascript implementations on all major browsers provide a powerful API for building interactive web pages. We want to use Javascript libraries, but program in Haskell, by using a Javascript monad.

A usable model can be built using a simple translation of a fixed set of function calls into Javascript commands. With careful construction, we can combine commands before sending them, optimizing network usage. The challenging part is having the Javascript return values in an efficient manner. Consider this Haskell code:

```
c <- getContext "my-canvas"
... some use of c ...
```

In a simple transaction model, `getContext` invokes a Javascript command on the client, returning the response as `c`. However, we would prefer the whole code fragment to be compiled to Javascript such that the binding and use of `c` are performed on the client directly, with no intermediate client<->server communication. And thanks to the ideas inside Unimo and `operational` we can!

We do this by constraining the returned values of all the primitives to be reifiable via a constraint on GADT constructors. In (the simplified version of) our Javascript compiler, Javascript function calls are implemented with the `JS_Call` primitive.

```
data JSInst :: * -> * where
  JS_Call     :: (Sunroof a) => String -> [JSValue] -> JSInst a
  ...
```

This is the key step, the `Sunroof` constraint provides the ability to generate a prototypical `a`. The Unimo trick works for constraint types as well as monomorphic types.

So, from our list of primitives, the `operational` package allows us to build our Javascript monad, with the monad instance for `JSM` is provided `Program`.

```
type JSM a = Program JSInst a
```

For technical reasons, `Program` is abstract in `operational`, so the library provides `view` to give a normalized form of the monadic code. In the case of `JS_Call`, bind corresponds to normal sequencing, where the result of the function call is assigned to a variable, whose name has already been passed to the rest of the computation for compilation. `newVar`, `assignVar` and `showVar` are provided by the `Sunroof` class.

```
compile :: Sunroof c => JSM c -> CompM String
compile = eval . view
  where
    showArgs :: [JSValue] -> String
    showArgs = intercalate "," . map show
    eval :: Sunroof b
         => ProgramView JSInst b -> CompM String
    eval (JS_Call nm args :>>= g) = do
       a <- newVar
       code <- compile (g a)
       return $ assignVar a ++ nm ++ "("
                ++ showArgs args ++ ");" ++ code
    ...
    eval (Return b) = return $ showVar b
```

This critically depends on the type-checking extensions used for compiling GADTs, and scales to additional primitives, provided they are constrained on their polymorphic result, like `JS_Call`.

Using `compile`, we compile our Sunroof Javascript DSL to Javascript, and now a bind in Haskell results in a value binding in Javascript. A `send` command compiles the Javascript expressed in monadic form and sends it to the browser for execution.

```
send :: (Sunroof a) => JSM a -> IO a
```

The Javascript code then responds with the return value, which can be used as an argument to future calls to `send`.

We can write a trivial example which draws a circle that follows the mouse:

```
drawing_app :: Document -> IO ()
drawing_app doc = do
  ...
  send doc $ loop $ do
        event <- waitFor "mousemove"
        let (x,y) = (event ! "x",event ! "y")
        c <- getContext "my-canvas"
        c <$> beginPath()
        c <$> arc(x, y, 20, 0, 2 * pi, false)
        c <$> fillStyle := "#8ED6FF"
        c <$> fill()
```

The following code is generated by Sunroof (on the Haskell server) and then executed entirely on the client:

```
var loop0 = function(){
  waitFor("mousemove",function(v1){
    var v2=getContext("my-canvas");
    (v2).beginPath();
    (v2).arc(v1["x"],v1["y"],20,0,2*Math.PI,false);
    (v2).fillStyle = "#8ED6FF";
    (v2).fill();
    loop0();
  })
}; loop0();
```

Volia! A Haskell-based Javascript monad reified and transmitted to a browser.

## Close

This blog article is adapted from the short paper [Haskell DSLs for Interactive Web Services][5], submitted to [XLDI 2012][6], written by Andrew Farmer and myself.

The lesson, I suppose, is never assume something is not possible in Haskell. We only stumbled onto this when we were experimenting with a variant of monadic bind with class constraints, and managed to remove the constraints. We’ve never seen an example of using `operational` or Unimo that constraints the primitives to be able to generate specifically a prototypical value, aka the function reification trick above. If anyone has seen this, please point it out, and we’ll be happy to cite it.

We would like to thank Heinrich Apfelmus for pointing out that we could rework our compiler to use `operational`, and providing us with suitable template of its usage.

Let the reification of monads begin!

Andy Gill

This entry was posted on Sunday, May 20th, 2012 at 12:10 am and is filed under [DSL][7], [Javascript][8], [Monads][9], [Tutorial][10]. You can follow any responses to this entry through the [RSS 2.0][11] feed. You can [leave a response][12], or [trackback][13] from your own site.

[1]: https://web.archive.org/web/20121105051310/http://conal.net/papers/jfp-saig/
[2]: https://web.archive.org/web/20121105051310/http://www.ittc.ku.edu/csdl/fpg/Tools/KansasLava
[3]: https://web.archive.org/web/20121105051310/http://dl.acm.org/citation.cfm?id=1159840
[4]: https://web.archive.org/web/20121105051310/http://hackage.haskell.org/package/operational
[5]: https://web.archive.org/web/20121105051310/http://www.ittc.ku.edu/csdl/fpg/sites/default/files/Sunroof-XLDI-Submitted.pdf
[6]: https://web.archive.org/web/20121105051310/http://workshops.inf.ed.ac.uk/xldi2012/
[7]: https://web.archive.org/web/20121105051310/http://www.ittc.ku.edu/csdlblog/?cat=12 "View all posts in DSL"
[8]: https://web.archive.org/web/20121105051310/http://www.ittc.ku.edu/csdlblog/?cat=14 "View all posts in Javascript"
[9]: https://web.archive.org/web/20121105051310/http://www.ittc.ku.edu/csdlblog/?cat=13 "View all posts in Monads"
[10]: https://web.archive.org/web/20121105051310/http://www.ittc.ku.edu/csdlblog/?cat=10 "View all posts in Tutorial"
[11]: https://web.archive.org/web/20121105051310/http://www.ittc.ku.edu/csdlblog/?feed=rss2&p=88
[12]: https://web.archive.org/web/20121105051310/http://www.ittc.ku.edu/csdlblog/?p=88#respond
[13]: https://web.archive.org/web/20121105051310/http://www.ittc.ku.edu/csdlblog/wp-trackback.php?p=88
