# GHC: Syntactic extensions

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#unicode-syntax

## UnicodeSyntax

The language extension `-XUnicodeSyntax` enables Unicode characters to be used to stand for certain ASCII character sequences.

The following alternatives are provided:

Name                      Code point  ASCII  Unicode

FOR ALL                       0x2200  forall   ∀
PROPORTION                    0x2237  ::       ::
BLACK STAR                    0x2605  *        ★
RIGHTWARDS DOUBLE ARROW       0x21D2  =>       ⇒

LEFTWARDS ARROW               0x2919  ->       →
RIGHTWARDS ARROW              0x2192  <-       ←

LEFTWARDS ARROW-TAIL          0x291A  -<       ↢
RIGHTWARDS ARROW-TAIL         0x291B  >-       ↣

LEFTWARDS DOUBLE ARROW-TAIL   0x291B  -<<      (-)
RIGHTWARDS DOUBLE ARROW-TAIL  0x291C  >>-      (-)

n >=>, <=<, <=>, <->
>->, <-<, <->, <=>
~=
(||)  (| |)

~> ({ὣ}) ~> ({ᾤ}) ~~> ({ὅ}) <~ ({õ}) *== *-- *~~ ~%

=:=
.=
.-:
~>
<~>
~~>
~-
~-
->>
<<-
<->
<|<|
||>|>
_|_
..
././//
.www.
... ....
-< >- >~ ~< >>- <<- % ->>
## %% && ***
][
[-]
[+]
{|}
]|[
{+}
<+>
<*>
<->
</>
<$>
<|>

(+)
(-)
(*)
(/)










* || >>- <<< >> >>> >< <|>
* ->> < ==> === !== -< ->
* *** </ />  |$| .%. :: ... !? ?? !! <==  --> <<| /= {|} {| <| |> 
* <> <| |> >< >%< >~< <><><> <>  @

Added U+239B..U+23AD multi-line brackets [#675]


⎛ 1 ⎞ ⎡ a ⎤ ⎧ x ⎫
⎜ 2 ⎟ ⎢ b ⎥ ⎪ y ⎪
⎜ 3 ⎟ ⎢ c ⎥ ⎨ z ⎬
⎜ 4 ⎟ ⎢ d ⎥ ⎪ y ⎪
⎝ 5 ⎠ ⎣ e ⎦ ⎩ x ⎭

U+27E8 ⟨ U+27E9 ⟩ [#763]

U+2262 ≢

U+2234 Therefore ∴ and U+2235 Because ∵ [#669]

Added U+27F0..U+27FF Supplemental Arrows-A 
⟲ ⟳
⟰ ⟱ 
⟴ 
⟵ ⟶
⟷ 
⟸ ⟹ ⟺ ⟻ ⟼ 
⟽ ⟾ 
⟿ 
[#677]

U+2400..U+2426 Control Pictures [#764]
␆
␈
␇
␣
␢ ␘ ␍
␐
␡
␥ ␔ ␑ ␓ ␒ ␙ ␃ ␄ ␗ ␅ ␛ ␜ ␌ ␝ ␉ ␊ ␕ ␤ ␀ ␞ ␏ ␎ ␠ ␁ ␂ 
␚ ␦ ␖ ␟ ␋

Added few technical symbols [#799]:
U+21A9 Leftwards arrow with hook (Return) ↩
U+21DE Upwards arrow with double stroke (Page Up) ⇞
U+21DF Downwards arrow with double stroke (Page down) ⇟
U+21E4 Left Tab Arrow ⇤
U+21E5 Right Tab Arrow ⇥
U+2300 Diameter sign ⌀
U+2303 Up Arrowhead (Control) ⌃
U+2304 Down Arrowhead ⌄
U+2305 Projective ⌅
U+2306 Perspective ⌆
U+2318 Place of interest sign (Command) ⌘
U+2324 Up Arrowhead between two horizontal bars ⌤
U+2325 Option Key ⌥
U+2387 Alternative Key Symbol ⎇
U+238B Broken Circle with Northwest Arrow (Escape) ⎋
U+23CF Eject symbol ⏏

Added Coq logical and /\ and logical or \/ ligatures, 
U+2227 ∧ and U+2228 ∨ [#191] [#488] [#738] [#810]

∧↑∧⇒∩∈⊆∀⊥¬∧⊕∨↓⇔∪∉⊂∃⊥≡∅

Disabled ligatures after regexp lookahead/lookbehinds (?<=< (?<=> (?<==> (?<=| (?<== (?=:= (?=!= (?== (?=== (?==> (?=> (?=>> (?=<< (?=/= (?!! (?!!. (?!= (?!== (?<!! (?<!!. (?<!= (?<!== (?<!-- [#578]
Removed ..= [#757]
Alternatives (stylistic sets):
Lowercase r (ss01) [#601]
Less than/greater than <= >= (ss02) [#263] [#617]
Ampersand & (ss03) [#617]
Dollar sign $ (ss04) [#617]
At sign @ (ss05) [#617] [#748] [#817]
Thin backslash (ss06) [#577 #720 #825]
Dotted zero 0 (zero, ss19)
Old-style figures (onum, ss20) [#561] [#715]
Old-style figures no longer prevent ligatures [#561] [#715]

Redrew {| |} [| |] ([#643])

Disabled ligatures in (?= 
(?<= (?: ([#624]), >=< ([#548]), {|} [|] ([#593])
`> = <`
