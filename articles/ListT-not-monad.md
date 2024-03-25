---
downloaded:       2022-01-03
page-url:         http://blog.sigfpe.com/2006/11/why-isnt-listt-monad.html
page-title:       A Neighborhood of Infinity: Why isn't ListT [] a monad?
article-title:    Why isn't ListT [] a monad?
---
# A Neighborhood of Infinity: Why isn't ListT [] a monad?

Well I'm back from my vacation. But this isn't my personal blog so I think it's time to dive right in. Anything to take my mind off these mosquito bites...
Well I'm back from my [vacation][1]. But this isn't my personal blog so I think it's time to dive right in. Anything to take my mind off these mosquito bites...

So consider the free semiring R generated by some set S. In other words, S consists of finite sums and products of 0, 1 and elements of S and where the only simplification rules allowed are

>   
> a+0=a, a+b=b+a, a+(b+c)=(a+b)+c
> 
> a1=1, a(bc) = (ab)c
> 
> a(b+c) = ab+ac and (a+b)c = ac+bc.

For example, consider a term like ab+c(ef+gh). We can use distributivity to multiply this out to ab+cef+cgh. There's not much else we can do to simplify this. Now notice that if you multiply out all of the brackets wherever you can, ie. use distributivity until you can no longer, you end up with an expression that is a sum of terms and each term is a product of generators from S. (The sum of terms may be empty, in which case it's zero, and each of the terms may be a product of zero generators, in which case it is 1). This allows us to write elements of R in a canonical form: as a set of terms where each term is an ordered list of generators. For example, we could write ab+cef+cgh as {\[a,b\],\[c,e,f\],\[c,g,h\]}. Notice how the commutativity of addition is represented by the fact that we're using a *set* of terms, but each term is a *list* of generators because we're making no assumption about the commutativity of multiplication.

In Haskell it's more convenient to work with lists. Se we'll represent our running example as \[\['a','b'\],\['c','e','f'\],\['c','g','h'\]\]. So if S is the Haskell type corresponding to our set of generators, then \[\[S\]\] can be thought of as the free semiring generated by elements of S, with the proviso that we consider two elements equal if one can be reordered to the other. Note that \[\]=0 and \[\[\]\] = 1.

Now suppose that S is itself a free semiring generated by T. Then R = \[\[ \[\[T\]\] \]\], modulo ordering. If you think about it, there's a nice way to use the algebraic structure to 'flatten' an element of \[\[ \[\[T\]\] \]\] down to an element of \[\[T\]\]. R is freely generated by elements of S, in other words it consists of sums of products of elements of S. But the elements of S are themselves sums and products. So we have sums of *products* of *sums* of products. I emphasised two words in that sentence. This is because R contains subparts that are products of sums. If we multiply these out in the usual way, we get sums of sums of products of products. Group the sums and products together, and we get back to sums of products. Here's an example: any element of T trivially gives an element of S. So if a,b,c,e,f,g and h are all in T, then a,b,c and ef+gh are all in S and hence are generators of R, so ab+c(ef+gh) is in R. Multiply out and we obviously have the element ab+cef+cgh of S. It's not hard to see how this generalises to map any element of S back down to T. So we have maps

>   
> T -> \[\[T\]\] (by trivial inclusion of generators)\[\[ \[\[T\]\] \]\] -> \[\[T\]\] (by above argument)

Look to you like a monad? Of course it does! :-) But what monad is it?

First, let's implement the map

\[\[ \[\[T\]\] \]\] -> \[\[T\]\]

. After tinkering I came up with

  
\> import Control.Monad.List

\> flatten x = concatMap (map concat . sequence) x

We can test it out

  
\> x = \[\[ \[\['a'\]\],\[\['b'\]\] \], \[ \[\['c'\]\],\[\['e','f'\],\['g','h'\]\] \]\]  
\> test1 = flatten x  

It multiplies out exactly the way we want.

Now compare with running this:

  
\> x' = ListT \[\[ ListT \[\['a'\]\],ListT \[\['b'\]\] \],  
\>           \[ ListT \[\['c'\]\],ListT \[\['e','f'\],\['g','h'\]\] \]\]  
\> test2 = runListT $ join x'  

In other words, apart from the

ListT

fluff,

join

for

ListT \[\]

is

flatten

. So if we consider lists as a mechanism to represent sets,

ListT \[\]

is revealed as the monad of semirings. I'm not sure, but I think that historically this is where monads originally came from. Certainly there are many papers on the relationship between monads and algebraic structures like semirings.

And now I can answer my original question. In a semiring, addition is commutative, so the order of terms doesn't matter. But in

ListT \[\]

, we're using lists, and order does matter in a list. So if we *do* take order into account, then really

ListT \[\]

is the monad of semirings where both addition and multiplication is non-commutative. And here's the problem: in general, there is no such thing as a freely generated semiring with non-commutative addition.

Here's why: consider the expression ((a+b)+(a+b))\*((a+b)+(a+b)). Multiply out the inner parentheses first and we get

>   
> (a+b+a+b)\*(a+b+a+b)
> 
> \= a\*a+a\*b+a\*a+a\*b+…

Now multiply out the outer parentheses first and we get

>   
> (a+b)\*(a+b)+(a+b)\*(a+b)+(a+b)\*(a+b)+(a+b)\*(a+b)
> 
> \= a\*a+a\*b+b\*a+b\*b+…

The terms are coming out in a different order. Essentially distributivity doesn't work in a semiring with non-commutative addition.

This translates directly into failure of one of the monad laws. First write our expression as an object of type

ListT \[\]

:

u = a+b

  
\> u = ListT \[\["a"\],\["b"\]\]  

v = (a+b)+(a+b)

  
\> v = ListT \[\[u\],\[u\]\]  

w = ((a+b)+(a+b))\*((a+b)\*(a+b))

  
\> w = ListT \[\[v,v\]\]  

join

multiplies out parentheses. So working from the outer parentheses inwards we can use:

  
\> expanded1 = join $ join w  
\> go1 = runListT expanded1  

Working outwards from the inner parentheses we get:

  
\> expanded2 = join $ fmap join w  
\> go2 = runListT expanded2  

(Note how I use

fmap

to 'duck down' through the outer layer of parentheses so as to multiply out each of the subexpressions first.)

Evaluate

go1

and

go2

and you'll see that they corresponds to the two ways of multiplying out that I gave above. And more importantly, the values of

expanded1

and

expanded2

aren't equal, meaning that

join . join = join . fmap join

isn't satisfied. You may recognise this: it's one of the monad laws. (At least it's one of the monad laws written the way category theorists write them.) So we ain't got no monad.

---

I think this is now a fairly complete analysis of what

ListT \[\]

is all about. So one obvious remaining question is: where do [games][2] come into all this? The answer is that games form a semiring in a way that I haven't seen documented anywhere (though is surely common knowledge). I'd explain but I've run out of time...

Note that the reason

ListT \[\]

isn't a monad is that

\[\]

isn't commutative, in some sense. This has been observed many times in the past. Two papers mentioning this are [this][3] and [this][4].

I actually figured out all of this stuff before [this][5]. I realised that the trees I was scribbling on the backs of my envelopes to represent elements of semirings could actually be generalised to just about any kind of tree, so I wrote about the general case first.

I prefer my example of

ListT \[\]

failing to be a monad to the examples given [here][6]. The latter make use of the IO monad so they aren't quite as 'pure'.

Labels: [haskell][7], [mathematics][8]

[1]: http://homepage.mac.com/sigfpe/PhotoAlbum26.html
[2]: http://sigfpe.blogspot.com/2006/10/games-strategies-and-self-composition.html
[3]: http://citeseer.ist.psu.edu/king92combining.html
[4]: http://web.cecs.pdx.edu/~mpj/pubs/RR-1004.pdf
[5]: http://sigfpe.blogspot.com/2006/11/variable-substitution-gives.html
[6]: http://www.haskell.org/hawiki/ListTDoneRight
[7]: http://blog.sigfpe.com/search/label/haskell
[8]: http://blog.sigfpe.com/search/label/mathematics