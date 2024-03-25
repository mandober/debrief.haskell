# Combinators :: Table

Nr|Lambda abstraction    |A| Symbol| Bird                     | Combinator           | Haskell        | SK Combinator exp
--|----------------------|-|-------|--------------------------|----------------------|----------------|----------------------------
01|λabc.a(bc)            |3| B     | Bluebird                 | S(KS)K               | (.)            | B
02|λabcd.a(bcd)          |4| B₁    | Blackbird                | BBB                  |                | B
03|λabcde.a(bcde)        |5| B₂    | Bunting                  | B(BBB)B              |                | B
04|λabcd.a(b(cd))        |4| B₃    | Becard                   | B(BB)B               |                | B
06|λabcd.ab(cd)          |4| D     | Dove                     | BB                   |                | D
07|λabcde.abc(de)        |5| D₁    | Dickcissel               | B(BB)                |                | D
08|λabcde.a(bc)(de)      |5| D₂    | Dovekies                 | BB(BB)               |                | D
09|λabcde.ab(cde)        |5| E     | Eagle                    | B(BBB)               |                | E
10|λabcdefg.a(bcd)(efg)  |7| Ê     | Bald Eagle               | B(BBB)(B(BBB))       |                | E
12|λabcd.ad(bc)          |4| G     | Goldfinch                | BBC                  |                | G
13|λabc.abcb             |3| H     | Hummingbird              | BW(BC)               |                | H
15|λabcd.ab(adc)         |4| J     | Jay                      | B(BC)(W(BC(B(BBB)))) |                | J
16|λab.a                 |2| K     | Kestrel                  | K                    | const          | K
20|λab.b(ab)             |2| O     | Owl                      | SI, O g f = f (g f)  | (.)(.), BB     | O
21|λabc.b(ac)            |3| Q     | Queer     Bird           | CB                   | flip (.)       | Q
22|λabc.a(cb)            |3| Q₁    | Quixotic  Bird           | BCB                  |                | Q
23|λabc.b(ca)            |3| Q₂    | Quizzical Bird           | C(BCB)               |                | Q
24|λabc.c(ab)            |3| Q₃    | Quirky    Bird           | BT                   |                | Q
25|λabc.c(ba)            |3| Q₄    | Quacky    Bird           | F'B                  |                | Q
27|λabc.ac(bc)           |3| S     | Starling                 | S                    |                | S
28|λab.ba                |2| T     | Thrush                   | CI                   | (&)            | T
14|λa.a                  |1| I     | Idiot Bird               | SKK                  | id             | I
31|λab.abb               |2| W     | Warbler                  | C(BMR)               | W f x = f x x  | W
32|λab.baa               |2| W¹    | Converse Warbler         | CW                   | flip W         | W
05|λabc.acb              |3| C     | Cardinal                 | S(BBS)(KK)           | flip           | C
26|λabc.bca              |3| R     | Robin                    | BBT                  |                | R
11|λabc.cba              |3| F     | Finch                    | ETTET                |                | F
30|λabc.cab              |3| V     | Vireo                    | BCT                  |                | V
34|λab.ab                |2| I'    | Idiot Bird  Once Removed | S(SK)                | ($), id        | I
35|λabc.abcc             |3| W'    | Warbler     Once Removed | BW                   |                | W
36|λabcd.abdc            |4| C'    | Cardinal    Once Removed | BC                   |                | C
37|λabcd.acdb            |4| R'    | Robin       Once Removed | C'C'                 |                | R
38|λabcd.adcb            |4| F'    | Finch       Once Removed | BC'R'                |                | F
39|λabcd.acbd            |4| V'    | Vireo       Once Removed | C'F'                 |                | V
40|λabc.abc              |3| I''   | Idiot Bird Twice Removed | _                    | id             | I
41|λabcd.abcdd           |4| W''   | Warbler    Twice Removed | B(BW)                |                | W
42|λabcde.abced          |5| C''   | Cardinal   Twice Removed | BC'                  |                | C
43|λabcde.abdec          |5| R''   | Robin      Twice Removed | BR'                  |                | R
44|λabcde.abedc          |5| F''   | Finch      Twice Removed | BF'                  |                | F
45|λabcde.abecd          |5| V''   | Vireo      Twice Removed | BV'                  |                | V
