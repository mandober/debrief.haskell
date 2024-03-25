# Can I parameterise the empty constraint type?

> I have a class for queues which allows the instance to define the constraints it places on the elements. For example, a priority queue requires its elements to be orderable:

{-# LANGUAGE

Asked 6 years, 5 months ago

Viewed 457 times

I have a class for queues which allows the instance to define the constraints it places on the elements. For example, a priority queue requires its elements to be orderable:

    {-# LANGUAGE MultiParamTypeClasses, ConstraintKinds, FunctionalDependencies #-}
    
    class Queue q c | q -> c where
        empty :: q a
        qpop :: c a => q a -> Maybe (a, q a)
        qpush :: c a => a -> q a -> q a
    
    data PriorityQueue a = ...
    
    instance Queue PriorityQueue Ord where
        ...
    

This works a charm: inside the instance declaration for `PriorityQueue` I can operate on elements of the queue using members of `Ord` such as `(>)`.

* * *

I've got stuck trying to define a queue which places no requirements on its elements:

    newtype LIFO a = LIFO [a]
    
    instance Queue LIFO () where
        empty = LIFO []
        qpop (LIFO []) = Nothing
        qpop (LIFO (x:xs)) = Just (x, LIFO xs)
        qpush x (LIFO xs) = LIFO $ x:xs
    

This fails, with the following error message from GHC:

    The second argument of `Queue' should have kind `* -> Constraint',
      but `()' has kind `*'
    In the instance declaration for `Queue LIFO ()'
    

This error message makes sense to me. `Eq` accepts a type parameter (we typically write `Eq a => ...`) whereas `()` has no parameters - it's a plain old kind mismatch.

* * *

I had a crack at writing a type function which ignores its second argument, which would allow me to write `instance Queue LIFO (Const ())`:

    {-# LANGUAGE TypeFamilies, KindSignatures, PolyKinds #-}
    
    type family Const a b :: k -> k2 -> k
    type instance Const a b = a
    

I find this interaction of type families and kind polymorphism quite beautiful, so I was rather disappointed when it didn't work (I really thought it would!):

    Expecting two more arguments to `a'
    The first argument of `Const' should have kind `*',
      but `a' has kind `k0 -> k1 -> k0'
    In the type `a'
    In the type instance declaration for `Const'
    

I have a feeling this last example is something stupid like a syntax mistake (I'm new to type families). How can I write a `Constraint` which doesn't place any restrictions on its argument?

asked Mar 12 '15 at 13:24

[

![](https://www.gravatar.com/avatar/b3e0b5c8e42a8c38009ef2d969f8dabb?s=64&d=identicon&r=PG)

](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/users/1523776/benjamin-hodgson)

[Benjamin Hodgson](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/users/1523776/benjamin-hodgson)♦Benjamin Hodgson

38.1k16 gold badges101 silver badges147 bronze badges

4

This should work:

    class NoConstraint a where
    instance NoConstraint a where
    
    instance Queue LIFO NoConstraint where
      ...
    

The above defines a constraint which is satisfied by all types. As such, the obligations `c a` where `c = NoConstraint` can always be discharged. Also, since there are no members in that class, it should have zero (or nearly zero) run-time cost.

The "constraint" `()` you are trying to use is not seen as an empty constraint set by GHC, but as the unit type `() :: *`. This causes `Const () :: k2 -> *`, which triggers the kind error.

If you do not want to use a custom class, you might try e.g. `Const (Eq ())` or `Const (Num Int)`, which have the right kind `k2 -> Constraint`. I do not recommend this, though, since I find it less readable than using a custom class.

(This requires to enable some extensions, as Benjamin Hodgson points out below in a comment.)

answered Mar 12 '15 at 13:39

[

![](https://www.gravatar.com/avatar/93bb12ca0f93dfddb33880316f7e66c2?s=64&d=identicon&r=PG&f=1)

](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/users/3234959/chi)

[chi](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/users/3234959/chi)chi

103k3 gold badges115 silver badges191 bronze badges

4

Your Answer
-----------

*   Links
*   Images
*   Styling/Headers
*   Lists
*   Blockquotes
*   Code
*   HTML
*   Tables
*   [Advanced help](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/editing-help)

In most cases, a plain URL will be recognized as such and automatically linked:

Visit https://area51.stackexchange.com/ regularly!
Use angle brackets to force linking: Have you seen <https://superuser.com>?

To create fancier links, use Markdown:

Here's \[a link\](https://www.example.com/)! And a reference-style link to \[a panda\]\[1\].
References don't have to be \[numbers\]\[question\].

 \[1\]: https://notfound.stackexchange.com/
 \[question\]: https://english.stackexchange.com/questions/11481

You can add tooltips to links:

Click \[here\](https://diy.stackexchange.com "this text appears when you mouse over")!
This works with \[reference links\]\[blog\] as well.

 \[blog\]: https://stackoverflow.blog/ "click here for updates"

Images are exactly like links, but they have an exclamation point in front of them:

!\[a busy cat\](https://cdn.sstatic.net/Sites/stackoverflow/Img/error-lolcat-problemz.jpg)
!\[two muppets\]\[1\]

 \[1\]: https://i.imgur.com/I5DFV.jpg "tooltip"

The word in square brackets is the alt text, which gets displayed if the browser can't show the image. Be sure to include meaningful alt text for screen-reading software.

Be sure to use text styling sparingly; only where it helps readability.

\*This is italicized\*, and so
is \_this\_.

\*\*This is bold\*\*, just like \_\_this\_\_.

You can \*\*\*combine\*\*\* them
if you \_\_\_really have to\_\_\_.

To break your text into sections, you can use headers:

A Large Header
==============

Smaller Subheader
-----------------

Use hash marks if you need several levels of headers:

\# Header 1 #
## Header 2 ##
### Header 3 ###

Both bulleted and numbered lists are possible:

\- Use a minus sign for a bullet
+ Or plus sign
\* Or an asterisk

1. Numbered lists are easy
2. Markdown keeps track of
   the numbers for you
7. So this will be item 3.

1. Lists in a list item:
 \- Indented four spaces.
 \* indented eight spaces.
 \- Four spaces again.
2. You can have multiple
 paragraphs in a list items.
 
 Just be sure to indent.

\> Create a blockquote by
> prepending “>” to each line.
>
> Other formatting also works here, e.g.
>
> 1. Lists or
> 2. Headings:
>
> ## Quoted Heading ##

You can even put blockquotes in blockquotes:

\> A standard blockquote is indented
> > A nested blockquote is indented more
> > > > You can nest to any depth.

To create code blocks or other preformatted text, indent by four spaces or surround with groups of backticks:

 This will be displayed in a monospaced font. The first four spaces
 will be stripped off, but all other whitespace will be preserved.

\`\`\`
Markdown and HTML are turned off in code blocks:
<i>This is not italic</i>, and \[this is not a link\](https://example.com)
\`\`\`

To create not a block, but an inline code span, use backticks:

The \`$\` character is just a shortcut for \`window.jQuery\`.

If you want to have a preformatted block within a list, indent by eight spaces:

1\. This is normal text.
2. So is this, but now follows a code block:
 
 Skip a line and indent eight spaces.
 That's four spaces for the list
 and four to trigger the code block.

If you need to do something that Markdown can't handle, use HTML. Note that [we only support a very strict subset of HTML!](https://meta.stackexchange.com/questions/1777/what-html-tags-are-allowed)

Strikethrough humor is <strike>funny</strike>.

Markdown is smart enough not to mangle your span-level HTML:

<b>Markdown works \*fine\* in here.</b>

Block-level HTML elements have a few restrictions:

1.  They must be separated from surrounding text by blank lines.
2.  The begin and end tags of the outermost block element must not be indented.
3.  Markdown can't be used within HTML blocks.

  

<pre>
    You can <em>not</em> use Markdown in here.
</pre>

You can create tables using the [GitHub-flavored markdown format](https://github.github.com/gfm/#tables-extension-).

| A header | Another header |
| -------- | -------------- |
| First    | row            |
| Second   | row            |

*   A header row is required and must be followed by a separator row with the same number of cells
*   Cells are separated by a pipe (`|`) symbol

Set the **alignment** of a table column by placing a `:` on the left, right, or both sides of a separator in the separator line.

| left | center | right |
|:---- |:------:| -----:|
| One  | Two    | Three |

Not the answer you're looking for? Browse other questions tagged [haskell](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/questions/tagged/haskell "show questions tagged 'haskell'") [typeclass](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/questions/tagged/typeclass "show questions tagged 'typeclass'") [type-constraints](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/questions/tagged/type-constraints "show questions tagged 'type-constraints'") [type-families](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/questions/tagged/type-families "show questions tagged 'type-families'") or [ask your own question](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/questions/ask).
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


[Source](https://stackoverflow.com/questions/29011021/can-i-parameterise-the-empty-constraint-type?noredirect=1&lq=1)