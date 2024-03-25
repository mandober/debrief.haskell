# Haskell as a markup language
http://okmij.org/ftp/Haskell/HSXML/

We demonstrate that Haskell concisely represents semi-structured documents and the rules of their processing. In effect, Haskell can implement SXML (ssax.sourceforge.net), with its syntax and the extensible set of "tags". Haskell brings the benefit of static type guarantees, such as prohibiting an `H1` element to appear in the character content of other elements. The specification of various pre-post-order and context-sensitive traversals seems more concise in
Haskell than in Scheme.

The features of our framework are:

- extensibility: the user may add new tags, new contexts, and new transformation and traversal modes.

- static safety: there are no dynamics, no variant data types for elements or attributes, and thus no possibility of a run-time pattern-match failure.

- static guarantees: the framework statically ensures that a `block-level element` cannot appear in the `inline` content and that a `character-content entity` cannot appear in a pure `element content`. The framework does permit *context-polymorphism*: 'title' may be either a block-level element or an attribute. The generated XML or HTML document will not only be well-formed: it would also satisfy some validity constraints. More validity constraints can be added to the framework.

- flexibility of traversals: the same document can be transformed in pre-, post-, accumulating or other ways, even within the same session. A document can be processed in a pure function, or in a monadic action, including an IO action. In the latter case, we can, e.g. verify URLs as we generate an HTML document. A tag can be transformed depending of the transformation modes and tag's context: e.g., the same `(title "title")` expression may appear as an attribute or an element. It will be rendered differently by the (X)HMTL serializer.

- pleasant syntax, which closely resembles SXML. We can write `(p "string" "string1" br "string3")` without unnecessary commas and other syntactic distractions. We take polyvariadic functions to the new level (pun intended). We also exploit functional dependencies on a per-instance level, which is a very useful but, alas, rarely used technique.


The development of the framework went through several iterations. The earlier, 2006 version, represented semi-structured data truly as a heterogeneous data structure, an `HList`. Presently the framework follows the *final tagless* approach, and is reminiscent of the very first version, in which semi-structured data are represented as a monadic value polymorphic over the rendering monad. Instead of a monad however, we now use a monoid. To be precise, we use a monoid (called `MDoc`) with the operations to inject strings and to define markup (`emit_elem`, `emit_attr`). A simple String is an instance of such a monoid, useful for debugging. Another instance is `IO ()`, with `putStr` as a string-injection operation. This instance lets us output markup documents incrementally.

`MDoc` elements are built in a context:

```hs
newtype DC ctx d = DC { unDC :: d }
  deriving (Monoid, MDoc, TextD, StringMonoid)
```

The user-extensible set of contexts includes at least the standard HTML contexts: inline, block, attribute. When a marked-up document is being created, the context propagates outside in, with the element determining the context for its children.

The files `HSXML_doc.hs`, `HSXML.hs`, `HSXML_HTML.hs` and `Tables.hs` in this directory constitute the complete code for the framework; `sample1c.hs` is the sample code. The very first, monad-centric version is in one self-contained file `CSXML.hs`.

Our running example, inspired by the Haskell.org web site, is:

```hs
test_haskell =
  (document
    (head
      (title "Haskell" longdash "HaskellWiki")
      (meta_tag (description "All about the language" br "Haskell")))
  (body
    (h1 "Haskell")
    (div (attr (title "titleline"))
      (p
        (a (attr (href (FileURL "/haskellwiki/Image:Haskelllogo.jpg")))
          "Haskell" br "A <purely functional> language")
        br
      )
      (p "Haskell is a general purpose,"
        (em (strong "purely") "functional") "programming language"))))
```

The absence of commas may be striking. This Haskell code looks identically to SXML.

Markup `br` may be used in various contexts: in the character content of an element and of an attribute (cf. `description` for the latter). However, if we try to replace "Haskell" within the `description` attribute with `em "Haskell"` we get the type error:

```hs
Couldn't match `CT_attr' against `CT_inline'
  Expected type: CT_attr
    Inferred type: CT_inline
```

On the other hand, the string "Haskell" within the `h1` element may be replaced with `em "Haskell"`. If we try to nest `h1` as in `h1 (h1 "Haskell")`, we get the type error: "Couldn't match `CT_inline' against `CT_block'. Headers are not allowed in the `inline' context".

We can transform the `test_haskell` in many ways (e.g. extract all the titles, renumber sections, etc). We can also render it in HTML. The result is quite predictable:

```html
<!DOCTYPE HTML 5">
<html>
<head>
  <title>Haskell&mdash; HaskellWiki</title>
  <meta name="description" content="All about the language Haskell"></head>
<body bgcolor="#FFFFFF">
  <h1>Haskell</h1>
  <div title="titleline">
    <p>
      <a href="/haskellwiki/Image:Haskelllogo.jpg">Haskell<br>
        A &lt;purely functional&gt; language</a>
        <br>
    </p>
    <p>Haskell is a general purpose, <em><strong>purely</strong> 
      functional</em> programming language</p>
  </div>
</body>
</html>
```

The markup title, which can be either an element or an attribute, has indeed been rendered differently in different contexts.

Adding new markup is easy. For example, the markup `longdash`, `a`, and `title` are not defined in the base file `HSXML.hs`. We add these tags in `sample1c.hs`, as follows:

The long dash may appear in the character content of an element or an attribute

```hs
longdash :: (MDoc d, Check_ia longdash ct) => DC ct d
longdash = emit_lit "&mdash;"
```

We define `longdash` as a `StringMonoid` (a monoid with `emit_lit` method to inject strings) that may appear in contexts satisfying the class `Check_ia` (that is, in `CT_inline` and `CT_attr` contexts).

The anchor is an inline element with attributes and an inline content

```hs
a (DC attrs :: DC CT_block d) body = inline_inline tr mempty body
  where tr (DC body) = cdata_sep $
    DC $ emit_elem "a" (Just attrs) (Just body)
```

The inferred type

```hs
a :: (Build (DC CT_inline d) (DC CT_inline d) t, MDoc d) =>
     DC CT_block d -> DC CT_inline d -> t
```

makes it clear that `a` may appear only in inline context. The function `cdata_sep` marks the anchor document as the one with text content. When such documents are concatenated, an intervening space will be inserted.

Title can be either
- a block-level element whose content is `CT_inline`
- an attribute, whose content is, therefore, `CT_attr`

It is rendered context-sensitively:

```hs
class MkTitle ctx where
  mk_title :: MDoc d => DC CT_inline d -> DC ctx d

instance MkTitle CT_block where
  mk_title (DC body) = DC $ emit_elem "title" Nothing (Just body)

instance MkTitle CT_battr where
  mk_title (DC body) = DC $ emit_attr "title" body

title = from_inline mk_title dc_empty
```

It seems many of the SXML transformations (cf. the 'examples' directory of the SSAX distribution or CVS repository, ssax.sf.net) can be rendered in Haskell.
