# Comonad uses

- Streams
- Spreadsheets
- Trees

Comonads are infinite structures or infinite spaces (i.e. an infinite context). We can VIEW comonads from different FOCUSES (focused slots or points of view). A view may be incomplete - some elements may be missing from some views.

With streams, when we duplicate a stream (while focusing on the initial index or "slot"), we get a stream of streams, so each slot of the stream of streams gets some configuration of the original stream: the index 0 gets the entire original stream, at the index 1 the stream is the tail of the original stream, the index 2 has the tail of tail of the original stream, and so on. Duplication of a stream, like the `duplicate` operation on any comonad must satisfy the comonadic laws.

So the index 0 represents the *absolute view* into the stream, while all the subsequent indices represent *relative views* into the stream, such as the view from the 5th element (slot) and similar. These relative views have the original stream truncated from the start by some number of elements (perhaps exactly by the current `index` number of elements).

Non-empty trees are comonads (must have at least one node)
- slots are nodes
- views are subtrees
- `extract` is the root (absolute view) or a subtree's root (relative view)

Spreadsheets are comonads
- slots are cells
- views are relative
- `extract` is the focused cell

Nonempty lists are comonads
- slots are the elements
- views are list `tails`
- `extract` is list `head`

Zippers (tree, list)
- slots are the elements
- views are doubly-linked lists rooted at focused element
- `extract` is the focus of doubly-linked list

The slots are elements, and views are doubly linked lists rooted at that element. From the middle of the list, we can see the previous (earlier) list elements, as well as the subsequent (later) ones, and, importantly, we know which is which. The `extract` gets the focus of the list. The `duplicate` puts a zipper in each slot, and each zipper is focused on the slot that it is inside of. The duplicated zipper inside the slot `A` will have the focus of the doubly linked list on `A`, and similarly with `B`, `C`, etc.

Functions are comonads in certain cases (when paired with an `x`-axis value)
- slots: `x`-axis positions
- views: are relative to your `x` position
- `extract`: the `y` at your `x`

We need to have some starting position to know where we are in a function's graph (as the coordinate x,y system).

Functions without that x-value are not comonads, nor are list in general (non empty lists are), nor are `IO` computations - this time because `IO` doesn't allow you to extract the value, ever (unless enforcing it rudely with `GHC.IO.Unsafe.unsafePerformIO :: forall a. IO a -> a`).
