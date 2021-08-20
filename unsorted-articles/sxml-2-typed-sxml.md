http://okmij.org/ftp/Haskell/HSXML/

From ssax-sxml-bounces@lists.sourceforge.net Tue Dec 19 02:02:44 2006
To: ssax-sxml@lists.sourceforge.net
From: oleg-at-pobox.com
Date: Mon, 18 Dec 2006 18:02:01 -0800 (PST)
Subject: [ssax-sxml] Typed SXML
X-comment: [July 2011] refreshed URLs
List-Archive: <http://sourceforge.net/mailarchive/forum.php?forum=ssax-sxml>

   This message describes the old version 1.0. In the current version 3.0
   HSXML not longer uses square brackets and looks almost the same as SXML:
   see the examples in http://okmij.org/ftp/Haskell/HSXML/

SXML is the representation of semi-structured data (XML Infoset) as a
Scheme tree -- or, alternatively, as Scheme code. As such, an SXML
document can be traversed and evaluated. The result of either
operation is a list of queried for values, a transformed SXML
document, or a set of strings representing the rendered version of the
document, in HTML, XML, LaTeX, Wiki, etc. formats.

All that processing has been untyped so far. It is an interesting
question to find out to which extent SXML and the above
transformations are possible in a typed setting. Or if they are
possible at all short of experimental systems with quite advanced type
systems. How much hassle typed SXML processing imposes on the
programmer, how many type annotations need to be written. How long
type inference (if possible at all) and type checking may take. What
static guarantees typed SXML processing may bring and if they are
worth the added trouble.

This message reports on several experiments in Haskell to answer those
questions. The upshot: Haskell as it is can represent semi-structured
data in SXML-conforming syntax, with the extensible set of `tags' and
statically enforced content model restrictions. Querying, transforming
and advanced rendering into HTML and XML are possible. Experience of
writing (moderately complex, so far) web pages in HSXML shows that the
typed SXML can be used in practice.

The benefit of representing XML-like documents as a typed data
structure/Haskell code is static rejection of bad documents -- not
only those with undeclared tags but also those where elements appear
in wrong contexts. Using HTML-like HSXML as an example, a document
with H1 within the P element is rejected as invalid. No HSXML
transformation may introduce nested Hn or P elements: the code will
not compile otherwise due to a type error. Thus the generated XML or
HTML document will not only be well-formed but also will satisfy some
validity constraints. Many (all?) content model constraints of
HTML/XML DTD can be expressed and thus enforced in types. The types
also guide queries, transformations, and rendering, making them
context sensitive. One may admit `br' both in element and attribute
content -- but render it differently, e.g., as a newline in the latter
case. Specifying these context-sensitive transformations seems easier
in Haskell. The type inference of such expressive types is possible;
in fact, in most cases no type annotations are needed, and in
remaining rare cases the `annotations' take a form of special
top-level terms such as `as_block'. The type inference and type
checking does take time with GHC(i) (e.g., 15 secs for moderately
complex HSXML page; normally about 3 secs), although running the HSXML
transformation code is fast. GHC has never been optimized for
compilation speed, and is quite slow in general.

Static typing does not inhibit extensibility. The HSXML library user
may always define new `tags', new transformation rules and new
contexts. Nor does static typing limit the complexity of
transformations. The same document can be processed in pre-, post-,
accumulating or other ways, even within the same transformation
session. A document can be processed in a pure function, or in a
monadic action, including an IO action.  In the latter case, we can,
e.g., verify URLs as we generate the HTML document.


The complete code is available from
     http://okmij.org/ftp/Haskell/HSXML/
Unless otherwise noted, all files mentioned throughout this message
are contained in the above archive.


Here is the first HSXML example, inspired by the Haskell.org web site:

    (document
     (head
      [title "Haskell" longdash "HaskellWiki"]
      [meta_tag [description "All about the language" br "Haskell"]])
    (body
     [h1 "Haskell"]
     [div (attr [title "titleline"])
      [p
       [[a (attr [href (FileURL "/haskellwiki/Image:Haskelllogo.jpg")])
         "Haskell" br "A <purely functional> language"]]
       br
      ]
      [p "Haskell is a general purpose," 
       [[em [[strong "purely"]] "functional"]] "programming language"]]))

Incidentally, it is also valid SXML and can be processed as such,
e.g., for rendering to HTML. The corresponding Scheme code is in the
file sample1c.scm, to be contrasted with the Haskell code
sample1c.hs. 

As a quoted Scheme expression (in a Scheme system treating brackets
as parentheses, which is common), the above document yields a tree --
a heterogenous list containing, inter alia, other heterogenous
lists. As a Haskell expression, the document too evaluates to a
heterogeneous list whose subterms may include heterogeneous lists. One
should note Haskell lists are homogeneous and their notation is
[1,2] or 1:2:[]. Pleasantly HSXML does not require comma separators and
permits heterogenous elements. HSXML is also typed. The above
expression has no type annotations. Its type is inferred by the
compiler, and can be seen by loading sample1c.hs into GHCi and
asking the GHCi to show the type of the expression.

We see that `br' can be used in various contexts: in the character
content of an element and of an attribute (cf. `description' for the
latter). However, if we try to replace "Haskell" within the
`description' attribute with [[em "Haskell"]] we get an error that
    Couldn't match `CT_attr' against `CT_inline'
      Expected type: CT_attr
      Inferred type: CT_inline
Only certain things (strings and br, for example) are permitted as
attribute values.

On the other hand, the string "Haskell" that appears within the `h1' 
element may be replaced with [[em "Haskell"]]. However, if we try 
to enter
     (h1 [[h1 "Haskell"]]) 
we get the type error 
    Couldn't match `CT_inline' against `CT_block'
Indeed, the element `H1' is not allowed in the `inline' context and so
Hn elements can't nest.

These validation errors are detected when _compiling_ the HSXML code
or the code that transforms one HSXML expression into another. The
error is raised well before the HSXML transformation begins and well
before any output is created. That's the meaning of static
validation. The transformation code in HSXML/ includes no dynamics
and no variant data types for elements or attributes, and thus offers
no possibility of a run-time pattern-match failure.

We can transform the above HSXML data structure in many ways (e.g.,
extract all the titles, renumber sections, etc). We can also render it
in HTML. The result is quite predictable:

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN"
 "http://www.w3.org/TR/REC-html40/loose.dtd">
<html><head><title>Haskell&mdash; HaskellWiki</title>
<meta name="description" content="All about the language
Haskell"></head>
<body bgcolor="#FFFFFF"><h1>Haskell</h1>
<div title="titleline"><p><a href="/haskellwiki/Image:Haskelllogo.jpg">Haskell
<br>
A &lt;purely functional&gt; language</a>
<br>
</p>
<p>Haskell is a general purpose, <em><strong>purely</strong> functional</em> programming language</p></div></body></html>

The BR element that occurred in the character content of an attribute
and of an element was indeed rendered differently depending on the
context.

The small difference between Scheme and Haskell SXML processing is the
treatment of adjacent strings. The Haskell library inserts a space
between two adjacent strings or other inline-content elements such as
`em'. A special pseudo-element `nosp' suppresses that behavior. This
space-insertion has been borrowed from LAML, and proved quite
convenient.

More complex examples.

If SXML is included as a literal in Scheme code, we can use the Scheme
evaluator and quasiquotation as a `macro' facility, for example, to
avoid repeating common fragments:

      (define sxml-doc
        (let ((abstr "levels of abstraction")
	      (qu (lambda (x) `("`" ,x "'"))))
       `(p "In the words of John Reynolds, a type system is a "
          "syntactic discipline for enforcing " ,abstr ". "
	  "By " ,@(qu abstr) " we mean ...")))

The same is available in HSXML:

-- From http://www.cs.cmu.edu/~rwh/research.htm
what_is_a_type_system = as_block $
    p "In the words of John Reynolds, a type system is a"
      "syntactic discipline for enforcing" abstr dot
      "By" (qu abstr) "we mean the clean separation between conceptually"
      "distinct data objects that may, in a particular program or compiler,"
      "have the same or similar representations."
  where
    dot   = [[tspan nosp "."]]
    abstr = "levels of abstraction"
    qu x  = [[tspan "`" nosp x nosp "'"]]

The pseudo-elements tdiv and tspan can be used for splicing a set of
elements in the block or inline contexts. As before, we statically
assure context validity. The more complex example, due to shelarcy, is
discussed in the file sample-sxml-simple.hs.

The above examples are the simple instances of multi-stage re-writing,
from one SXML expression to another until it is finally
rendered. Authoring web pages (on this site) involves many such staged
re-writings, as can be seen in the authoring library code
SSAX/lib/SXML-to-HTML-ext.scm (Scheme) and HSXML_ext.hs (Haskell).

The code RSS.hs, described in HSXML-to-RSS.txt, demonstrates
non-trivial transformations of HSXML marked-up data -- rendering data
in HTML and RSS/XML. The resulting document has the structure
different from that of the original markup: hierarchies may be
flattened, some pieces of data rearranged among elements. Rendering of
a particular markup element may be truly context sensitive, e.g., by
pulling data from the parent element. Creating an RSS document further
requires `subordinate' HTML rendering. We also demonstrate markup
transformations by successive rewriting (aka, `higher-order tags') and
the easy definition of new tags.

Finally, sample-code.hs shows off extensibility: defining new
elements, new contexts, new traversal strategies. The goal 
is to include Haskell source code in an HSXML document, properly render it
_and_ the result of its evaluation. We indeed evaluate code fragments
quoted in the HSXML document.

In many cases semi-structured data to process are not authored by us,
as Haskell code, but rather received as data from a (remote)
client. One may still treat that data as code and use type checking as
validation, with the help of hs-plugins. That is an interesting area
that seems feasible and worth future research.

