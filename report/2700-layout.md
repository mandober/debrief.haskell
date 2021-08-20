# 2.7 Layout

Haskell permits the omission of the braces and semicolons used in several grammar productions, by using layout to convey the same information. This allows both layout-sensitive and layout-insensitive styles of coding, which can be freely mixed within one program. Because layout is not required, Haskell programs can be straightforwardly produced by other programs.

The effect of layout on the meaning of a Haskell program can be completely specified by adding braces and semicolons in places determined by the layout. The meaning of this augmented program is now layout insensitive.

Informally stated, the braces and semicolons are inserted as follows. The layout (or "off-side") rule takes effect whenever the open brace is omitted after the keyword where, let, do, or of. When this happens, the indentation of the next lexeme (whether or not on a new line) is remembered and the omitted open brace is inserted (the whitespace preceding the lexeme may include comments). For each subsequent line, if it contains only whitespace or is indented more, then the previous item is continued (nothing is inserted); if it is indented the same amount, then a new item begins (a semicolon is inserted); and if it is indented less, then the layout list ends (a close brace is inserted). If the indentation of the non-brace lexeme immediately following a where, let, do or of is less than or equal to the current indentation level, then instead of starting a layout, an empty list "{}" is inserted, and layout processing occurs for the current level (i.e. insert a semicolon or close brace). A close brace is also inserted whenever the syntactic category containing the layout list ends; that is, if an illegal lexeme is encountered at a point where a close brace would be legal, a close brace is inserted. The layout rule matches only those open braces that it has inserted; an explicit open brace must be matched by an explicit close brace. Within these explicit open braces, no layout processing is performed for constructs outside the braces, even if a line is indented to the left of an earlier implicit open brace.



Given these rules, a single newline may actually terminate several layout lists. Also, these rules permit:

```hs
f x = let a = 1; b = 2
          g y = exp2
       in exp1
```

making `a`, `b` and `g` all part of the same layout list.

As an example, Figure 2.1 shows a (somewhat contrived) module and Figure 2.2 shows the result of applying the layout rule to it.

Note in particular:
- the line beginning `}};pop`, where the termination of the previous line invokes 3 applications of the layout rule, corresponding to the depth (3) of the nested where clauses,
- the close braces in the `where clause` nested within the tuple and case expression, inserted because the end of the tuple was detected
- the close brace at the very end, inserted because of the column 0 indentation of the end-of-file token.



Figure 2.1: A sample program

```hs
module AStack (Stack, push, pop, top, size ) where

data Stack a = Empty | MkStack a (Stack a)

push :: a -> Stack a -> Stack a
push x s = MkStack x s

size :: Stack a -> Int
size s = length (stkToLst s)
  where
    stkToLst Empty = []
    stkToLst (MkStack x s) = x : xs
      where xs = stkToLst s

pop :: Stack a -> (a, Stack a)
pop (MkStack x s) =
  (x, case s of
    r -> i r
      where
      i x = x
  )
-- (pop Empty) is an error

top :: Stack a -> a
top (MkStack x s) = x
-- (top Empty) is an error
```



Figure 2.2: Sample program with layout expanded (semicolons infested)

```hs
module AStack (Stack, push, pop, top, size ) where
{
  data Stack a = Empty | MkStack a (Stack a);
  push :: a -> Stack a -> Stack a; push x s = MkStack x s;
  size :: Stack a -> Int;
  size s = length (stkToLst s) where
  {
    stkToLst Empty = [];
    stkToLst (MkStack x s) = x:xs where
    {
      xs = stkToLst s
    }
  };
  pop :: Stack a -> (a, Stack a);
  pop (MkStack x s) = (x, case s of { r -> i r where { i x = x } });
  top :: Stack a -> a; top (MkStack x s) = x
}
```
