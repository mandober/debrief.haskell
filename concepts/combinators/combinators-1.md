# Combinators


B     Bluebird
B₁    Blackbird
B₂    Bunting
B₃    Becard
C     Cardinal
D     Dove
D₁    Dickcissel
D₂    Dovekies
E     Eagle
Ê     Bald Eagle
F     Finch
G     Goldfinch
H     Hummingbird
I     Idiot Bird
J     Jay
K     Kestrel
L     Lark
M     Mockingbird
M₂    Double Mockingbird
O     Owl
Q     Queer Bird
Q₁    Quixotic Bird
Q₂    Quizzical Bird
Q₃    Quirky Bird
Q₄    Quacky Bird
R     Robin
S     Starling
T     Thrush
U     Turing
V     Vireo
W     Warbler
W¹    Converse Warbler
Y     Why Bird (Sage Bird)

I'    Idiot Bird Once Removed
W'    Warbler Once Removed
C'    Cardinal Once Removed
R'    Robin Once Removed
F'    Finch Once Removed
V'    Vireo Once Removed

I''   Idiot Bird Twice Removed
W''   Warbler Twice Removed
C''   Cardinal Twice Removed
R''   Robin Twice Removed
F''   Finch Twice Removed
V''   Vireo Twice Removed


### All bird symbols

B, B₁, B₂, B₃, C, D, D₁, D₂, E, Ê, F, G, H, I, J, K, L, M, M₂, 
O, Q, Q₁, Q₂, Q₃, Q₄, R, S, T, U, V, W, W¹, Y, 
I', W', C', R', F', V', 
I'', W'', C'', R'', F'', V'', 
KI, Ω, KM, C(KM), Θ

Bluebird, Blackbird, Bunting, Becard, Cardinal, Dove, Dickcissel, Dovekies, Eagle, Bald Eagle, Finch, Goldfinch, Hummingbird, Idiot Bird, Jay, Kestrel, Lark, Mockingbird, Double Mockingbird, Owl, Queer Bird, Quixotic Bird, Quizzical Bird, Quirky Bird, Quacky Bird, Robin, Starling, Thrush, Turing, Vireo, Warbler, Converse Warbler, Why Bird (Sage Bird), Idiot Bird Once Removed, Warbler Once Removed, Cardinal Once Removed, Robin Once Removed, Finch Once Removed, Vireo Once Removed, Idiot Bird Twice Removed, Warbler Twice Removed, Cardinal Twice Removed, Robin Twice Removed, Finch Twice Removed, Vireo Twice Removed, Kite, Omega, Konstant Mocker, Crossed Konstant Mocker, Theta


## Combinator birds

All:     
B, B₁, B₂, B₃, C, 
D, D₁, D₂, E, Ě, 
F, G, H, I, J, 
K, L, M, M¹, 
O, P, R, S, T, 
U, V, Y
Θ, Ω, 


B, B₁, B₂, B₃, C, D, D₁, D₂, E, Ě, F, G, H, I, J, K, L, M, N, O, P, R, S, T, V

Bluebird, Blackbird, Bunting, Becard
Cardinal
Dove, Dickcissel, Dovekies
Eagle, Bald Eagle
Finch
Goldfinch
Hummingbird
Idiot
Jay
Kestrel, (Kite)
Lark
Mockingbird, Double Mockingbird
Owl
Queer, Quixotic, Quizzical, Quirky, Quacky
Robin (R)
Starling (S)
Thrush (T)
Vireo (V)
Warbler (W), Converse Warbler (W¹)


```
I  := λa.a
K  := λab.a
KI := λab.b

B  := λabc.a(bc)       B := λfgx.f(gx)
C  := λabc.acb         C := λfxy.fyx
S  := λabc.ac(bc)      S := λfgx.fx(gx)
V  := λabc.cab         V := λabs.sab
T  := λab.ba,          T := λxf.fx

D  := λabcd.ab(cd)
E  := 
F  :=
G  :=
H  :=
O  := 
R  := 


L  := λab.a(bb)
M  := λa.aa
M₂ := λab.ab(ab)
```

## Table

Nr|Lambda abstraction    |A| Symbol| Bird                        | Combinator           | Haskell        | SK Combinator exp
--|----------------------|-|-------|-----------------------------|----------------------|----------------|----------------------------
01|λabc.a(bc)            |3| B     | Bluebird                    | S(KS)K               | (.)            | ((S(KS))K)
02|λabcd.a(bcd)          |4| B₁    | Blackbird                   | BBB                  |                | ((S(K((S(KS))K)))((S(KS))K))
03|λabcde.a(bcde)        |5| B₂    | Bunting                     | B(BBB)B              |                | ((S(K((S(K((S(KS))K)))((S(KS))K))))((S(KS))K))
04|λabcd.a(b(cd))        |4| B₃    | Becard                      | B(BB)B               |                | ((S(K((S(K((S(KS))K)))((S(KS))K))))((S(KS))K))
05|λabc.acb              |3| C     | Cardinal                    | S(BBS)(KK)           | flip           | ((S((S(K((S(KS))K)))S))(KK))
06|λabcd.ab(cd)          |4| D     | Dove                        | BB                   |                | (S(K((S(KS))K)))
07|λabcde.abc(de)        |5| D₁    | Dickcissel                  | B(BB)                |                | (S(K(S(K((S(KS))K)))))
08|λabcde.a(bc)(de)      |5| D₂    | Dovekies                    | BB(BB)               |                | ((S(K((S(KS))K)))(S(K((S(KS))K))))
09|λabcde.ab(cde)        |5| E     | Eagle                       | B(BBB)               |                | (S(K((S(K((S(KS))K)))((S(KS))K))))
10|λabcdefg.a(bcd)(efg)  |7| Ê     | Bald Eagle                  | B(BBB)(B(BBB))       |                | ((S(K((S(K((S(KS))K)))((S(KS))K))))(S(K((S(K((S(KS))K)))((S(KS))K)))))
11|λabc.cba              |3| F     | Finch                       | ETTET                |                | ((S(K((S((SK)K))(K((S(K(S((SK)K))))K)))))((S(K((S(K((S(KS))K)))((S(KS))K))))((S(K(S((SK)K))))K)))
12|λabcd.ad(bc)          |4| G     | Goldfinch                   | BBC                  |                | ((S(K((S(KS))K)))((S((S(K((S(KS))K)))S))(KK)))
13|λabc.abcb             |3| H     | Hummingbird                 | BW(BC)               |                | ((S(K((S(K(S((S(K((S((SK)K))((SK)K))))((S(K((S(KS))K)))((S(K(S((SK)K))))K))))))K)))(S(K((S((S(K((S(KS))K)))S))(KK)))))
14|λa.a                  |1| I     | Idiot Bird                  | SKK                  | id             | ((SK)K)
15|λabcd.ab(adc)         |4| J     | Jay                         | B(BC)(W(BC(B(BBB)))) |                | ((S(K(S(K((S((S(K((S(KS))K)))S))(KK))))))((S((S(K((S((SK)K))((SK)K))))((S(K((S(KS))K)))((S(K(S((SK)K))))K))))(K((S(K((S((S(K((S(KS))K)))S))(KK))))(S(K((S(K((S(KS))K)))((S(KS))K))))))))
16|λab.a                 |2| K     | Kestrel                     | K                    | const          | K
17|λab.a(bb)             |2| L     | Lark                        | CBM                  | N/A            | ((S((S(KS))K))(K((S((SK)K))((SK)K))))
18|λa.aa                 |1| M     | Mockingbird                 | SII                  | N/A            | ((S((SK)K))((SK)K))
19|λab.ab(ab)            |2| M₂    | Double Mockingbird          | BM                   | N/A            | (S(K((S((SK)K))((SK)K))))
20|λab.b(ab)             |2| O     | Owl                         | SI                   | (.)(.)         | (S((SK)K))
21|λabc.b(ac)            |3| Q     | Queer Bird                  | CB                   |                | ((S(K(S((S(KS))K))))K)
22|λabc.a(cb)            |3| Q₁    | Quixotic Bird               | BCB                  |                | ((S(K((S((S(K((S(KS))K)))S))(KK))))((S(KS))K))
23|λabc.b(ca)            |3| Q₂    | Quizzical Bird              | C(BCB)               |                | ((S(K(S((S(K((S((S(K((S(KS))K)))S))(KK))))((S(KS))K)))))K)
24|λabc.c(ab)            |3| Q₃    | Quirky Bird                 | BT                   |                | (S(K((S(K(S((SK)K))))K)))
25|λabc.c(ba)            |3| Q₄    | Quacky Bird                 | F'B                  |                | ((S(K((S((S(K((S(KS))K)))S))(KK))))((S(K(S((S(K((S((S(K((S(KS))K)))S))(KK))))((S(KS))K)))))K))
26|λabc.bca              |3| R     | Robin                       | BBT                  |                | ((S(K((S(KS))K)))((S(K(S((SK)K))))K))
27|λabc.ac(bc)           |3| S     | Starling                    | S                    |                | S
28|λab.ba                |2| T     | Thrush                      | CI                   | (&)            | ((S(K(S((SK)K))))K)
29|λab.b(aab)            |2| U     | Turing                      | LO                   | (fix)          | ((S(K(S((SK)K))))((S((SK)K))((SK)K)))
30|λabc.cab              |3| V     | Vireo                       | BCT                  |                | ((S(K((S((S(K((S(KS))K)))S))(KK))))((S(K(S((SK)K))))K))
31|λab.abb               |2| W     | Warbler                     | C(BMR)               |                | ((S(K(S((S(K((S((SK)K))((SK)K))))((S(K((S(KS))K)))((S(K(S((SK)K))))K))))))K)
32|λab.baa               |2| W¹    | Converse Warbler            | CW                   |                | ((S(K(S((S(K(S((S(K((S((SK)K))((SK)K))))((S(K((S(KS))K)))((S(K(S((SK)K))))K))))))K))))K)
33|λa.a(λa)              |1| Y     | Why Bird (Sage Bird)        | SLL                  | (fix)          | (((SS)K)((S(K((SS)(S((SS)K)))))K))
34|λab.ab                |2| I'    | Idiot Bird Once Removed     | S(SK)                | ($), id        | (S(SK))
35|λabc.abcc             |3| W'    | Warbler Once Removed        | BW                   |                | (S(K((S(K(S((S(K((S((SK)K))((SK)K))))((S(K((S(KS))K)))((S(K(S((SK)K))))K))))))K)))
36|λabcd.abdc            |4| C'    | Cardinal Once Removed       | BC                   |                | (S(K((S((S(K((S(KS))K)))S))(KK))))
37|λabcd.acdb            |4| R'    | Robin Once Removed          | C'C'                 |                | ((S(K((S((S(K((S(KS))K)))S))(KK))))(S(K((S((S(K((S(KS))K)))S))(KK)))))
38|λabcd.adcb            |4| F'    | Finch Once Removed          | BC'R'                |                | ((S(K(S(K((S((S(K((S(KS))K)))S))(KK))))))((S(K((S((S(K((S(KS))K)))S))(KK))))(S(K((S((S(K((S(KS))K)))S))(KK))))))
39|λabcd.acbd            |4| V'    | Vireo Once Removed          | C'F'                 |                | ((S(K((S((S(K((S(KS))K)))S))(KK))))((S(K(S(K((S((S(K((S(KS))K)))S))(KK))))))((S(K((S((S(K((S(KS))K)))S))(KK))))(S(K((S((S(K((S(KS))K)))S))(KK)))))))
40|λabc.abc              |3| I''   | Idiot Bird Twice Removed    | _                    | id             | ()
41|λabcd.abcdd           |4| W''   | Warbler Twice Removed       | B(BW)                |                | (S(K(S(K((S(K(S((S(K((S((SK)K))((SK)K))))((S(K((S(KS))K)))((S(K(S((SK)K))))K))))))K)))))
42|λabcde.abced          |5| C''   | Cardinal Twice Removed      | BC'                  |                | (S(K(S(K((S((S(K((S(KS))K)))S))(KK))))))
43|λabcde.abdec          |5| R''   | Robin Twice Removed         | BR'                  |                | (S(K((S(K((S((S(K((S(KS))K)))S))(KK))))(S(K((S((S(K((S(KS))K)))S))(KK)))))))
44|λabcde.abedc          |5| F''   | Finch Twice Removed         | BF'                  |                | (S(K((S(K(S(K((S((S(K((S(KS))K)))S))(KK))))))((S(K((S((S(K((S(KS))K)))S))(KK))))(S(K((S((S(K((S(KS))K)))S))(KK))))))))
45|λabcde.abecd          |5| V''   | Vireo Twice Removed         | BV'                  |                | (S(K((S(K((S((S(K((S(KS))K)))S))(KK))))((S(K(S(K((S((S(K((S(KS))K)))S))(KK))))))((S(K((S((S(K((S(KS))K)))S))(KK))))(S(K((S((S(K((S(KS))K)))S))(KK)))))))))
46|λab.b                 |2| KI    | Kite                        | KI                   | flip const     | (K((SK)K))
47|λ                     |0| Ω     | Omega                       | MM                   | N/A            | (((S((SK)K))((SK)K))((S((SK)K))((SK)K)))
48|λab.bb                |2| KM    | Konstant Mocker             | KM                   | N/A            | (K((S((SK)K))((SK)K)))
49|λab.aa                |2| C(KM) | Crossed Konstant Mocker     | C(KM)                | N/A            | ((S(K(S(K((S((SK)K))((SK)K))))))K)
50|λ                     |0| Θ     | Theta                       | YO                   | N/A            | ((((SS)K)((S(K((SS)(S((SS)K)))))K)(S((SK)K))))



## Table backup

Combinator Birds

Nr|Lambda abstraction    |A| Symbol| Bird                        | Combinator           | Haskell        | SK Combinator exp
--|----------------------|-|-------|-----------------------------|----------------------|----------------|----------------------------
01|λabc.a(bc)            |3| B     | Bluebird                    | S(KS)K               | (.)            | ((S(KS))K)
02|λabcd.a(bcd)          |4| B₁    | Blackbird                   | BBB                  |                | ((S(K((S(KS))K)))((S(KS))K))
03|λabcde.a(bcde)        |5| B₂    | Bunting                     | B(BBB)B              |                | ((S(K((S(K((S(KS))K)))((S(KS))K))))((S(KS))K))
04|λabcd.a(b(cd))        |4| B₃    | Becard                      | B(BB)B               |                | ((S(K((S(K((S(KS))K)))((S(KS))K))))((S(KS))K))
05|λabc.acb              |3| C     | Cardinal                    | S(BBS)(KK)           | flip           | ((S((S(K((S(KS))K)))S))(KK))
06|λabcd.ab(cd)          |4| D     | Dove                        | BB                   |                | (S(K((S(KS))K)))
07|λabcde.abc(de)        |5| D₁    | Dickcissel                  | B(BB)                |                | (S(K(S(K((S(KS))K)))))
08|λabcde.a(bc)(de)      |5| D₂    | Dovekies                    | BB(BB)               |                | ((S(K((S(KS))K)))(S(K((S(KS))K))))
09|λabcde.ab(cde)        |5| E     | Eagle                       | B(BBB)               |                | (S(K((S(K((S(KS))K)))((S(KS))K))))
10|λabcdefg.a(bcd)(efg)  |7| Ê     | Bald Eagle                  | B(BBB)(B(BBB))       |                | ((S(K((S(K((S(KS))K)))((S(KS))K))))(S(K((S(K((S(KS))K)))((S(KS))K)))))
11|λabc.cba              |3| F     | Finch                       | ETTET                |                | ((S(K((S((SK)K))(K((S(K(S((SK)K))))K)))))((S(K((S(K((S(KS))K)))((S(KS))K))))((S(K(S((SK)K))))K)))
12|λabcd.ad(bc)          |4| G     | Goldfinch                   | BBC                  |                | ((S(K((S(KS))K)))((S((S(K((S(KS))K)))S))(KK)))
13|λabc.abcb             |3| H     | Hummingbird                 | BW(BC)               |                | ((S(K((S(K(S((S(K((S((SK)K))((SK)K))))((S(K((S(KS))K)))((S(K(S((SK)K))))K))))))K)))(S(K((S((S(K((S(KS))K)))S))(KK)))))
14|λa.a                  |1| I     | Idiot Bird                  | SKK                  | id             | ((SK)K)
15|λabcd.ab(adc)         |4| J     | Jay                         | B(BC)(W(BC(B(BBB)))) |                | ((S(K(S(K((S((S(K((S(KS))K)))S))(KK))))))((S((S(K((S((SK)K))((SK)K))))((S(K((S(KS))K)))((S(K(S((SK)K))))K))))(K((S(K((S((S(K((S(KS))K)))S))(KK))))(S(K((S(K((S(KS))K)))((S(KS))K))))))))
16|λab.a                 |2| K     | Kestrel                     | K                    | const          | K
17|λab.a(bb)             |2| L     | Lark                        | CBM                  | N/A            | ((S((S(KS))K))(K((S((SK)K))((SK)K))))
18|λa.aa                 |1| M     | Mockingbird                 | SII                  | N/A            | ((S((SK)K))((SK)K))
19|λab.ab(ab)            |2| M₂    | Double Mockingbird          | BM                   | N/A            | (S(K((S((SK)K))((SK)K))))
20|λab.b(ab)             |2| O     | Owl                         | SI                   | (.)(.)         | (S((SK)K))
21|λabc.b(ac)            |3| Q     | Queer Bird                  | CB                   |                | ((S(K(S((S(KS))K))))K)
22|λabc.a(cb)            |3| Q₁    | Quixotic Bird               | BCB                  |                | ((S(K((S((S(K((S(KS))K)))S))(KK))))((S(KS))K))
23|λabc.b(ca)            |3| Q₂    | Quizzical Bird              | C(BCB)               |                | ((S(K(S((S(K((S((S(K((S(KS))K)))S))(KK))))((S(KS))K)))))K)
24|λabc.c(ab)            |3| Q₃    | Quirky Bird                 | BT                   |                | (S(K((S(K(S((SK)K))))K)))
25|λabc.c(ba)            |3| Q₄    | Quacky Bird                 | F'B                  |                | ((S(K((S((S(K((S(KS))K)))S))(KK))))((S(K(S((S(K((S((S(K((S(KS))K)))S))(KK))))((S(KS))K)))))K))
26|λabc.bca              |3| R     | Robin                       | BBT                  |                | ((S(K((S(KS))K)))((S(K(S((SK)K))))K))
27|λabc.ac(bc)           |3| S     | Starling                    | S                    |                | S
28|λab.ba                |2| T     | Thrush                      | CI                   | (&)            | ((S(K(S((SK)K))))K)
29|λab.b(aab)            |2| U     | Turing                      | LO                   | (fix)          | ((S(K(S((SK)K))))((S((SK)K))((SK)K)))
30|λabc.cab              |3| V     | Vireo                       | BCT                  |                | ((S(K((S((S(K((S(KS))K)))S))(KK))))((S(K(S((SK)K))))K))
31|λab.abb               |2| W     | Warbler                     | C(BMR)               |                | ((S(K(S((S(K((S((SK)K))((SK)K))))((S(K((S(KS))K)))((S(K(S((SK)K))))K))))))K)
32|λab.baa               |2| W¹    | Converse Warbler            | CW                   |                | ((S(K(S((S(K(S((S(K((S((SK)K))((SK)K))))((S(K((S(KS))K)))((S(K(S((SK)K))))K))))))K))))K)
33|λa.a(λa)              |1| Y     | Why Bird (Sage Bird)        | SLL                  | (fix)          | (((SS)K)((S(K((SS)(S((SS)K)))))K))
34|λab.ab                |2| I'    | Idiot Bird Once Removed     | S(SK)                | ($), id        | (S(SK))
35|λabc.abcc             |3| W'    | Warbler Once Removed        | BW                   |                | (S(K((S(K(S((S(K((S((SK)K))((SK)K))))((S(K((S(KS))K)))((S(K(S((SK)K))))K))))))K)))
36|λabcd.abdc            |4| C'    | Cardinal Once Removed       | BC                   |                | (S(K((S((S(K((S(KS))K)))S))(KK))))
37|λabcd.acdb            |4| R'    | Robin Once Removed          | C'C'                 |                | ((S(K((S((S(K((S(KS))K)))S))(KK))))(S(K((S((S(K((S(KS))K)))S))(KK)))))
38|λabcd.adcb            |4| F'    | Finch Once Removed          | BC'R'                |                | ((S(K(S(K((S((S(K((S(KS))K)))S))(KK))))))((S(K((S((S(K((S(KS))K)))S))(KK))))(S(K((S((S(K((S(KS))K)))S))(KK))))))
39|λabcd.acbd            |4| V'    | Vireo Once Removed          | C'F'                 |                | ((S(K((S((S(K((S(KS))K)))S))(KK))))((S(K(S(K((S((S(K((S(KS))K)))S))(KK))))))((S(K((S((S(K((S(KS))K)))S))(KK))))(S(K((S((S(K((S(KS))K)))S))(KK)))))))
40|λabc.abc              |3| I''   | Idiot Bird Twice Removed    | _                    | id             | ()
41|λabcd.abcdd           |4| W''   | Warbler Twice Removed       | B(BW)                |                | (S(K(S(K((S(K(S((S(K((S((SK)K))((SK)K))))((S(K((S(KS))K)))((S(K(S((SK)K))))K))))))K)))))
42|λabcde.abced          |5| C''   | Cardinal Twice Removed      | BC'                  |                | (S(K(S(K((S((S(K((S(KS))K)))S))(KK))))))
43|λabcde.abdec          |5| R''   | Robin Twice Removed         | BR'                  |                | (S(K((S(K((S((S(K((S(KS))K)))S))(KK))))(S(K((S((S(K((S(KS))K)))S))(KK)))))))
44|λabcde.abedc          |5| F''   | Finch Twice Removed         | BF'                  |                | (S(K((S(K(S(K((S((S(K((S(KS))K)))S))(KK))))))((S(K((S((S(K((S(KS))K)))S))(KK))))(S(K((S((S(K((S(KS))K)))S))(KK))))))))
45|λabcde.abecd          |5| V''   | Vireo Twice Removed         | BV'                  |                | (S(K((S(K((S((S(K((S(KS))K)))S))(KK))))((S(K(S(K((S((S(K((S(KS))K)))S))(KK))))))((S(K((S((S(K((S(KS))K)))S))(KK))))(S(K((S((S(K((S(KS))K)))S))(KK)))))))))
46|λab.b                 |2| KI    | Kite                        | KI                   | flip const     | (K((SK)K))
47|λ                     |0| Ω     | Omega                       | MM                   | N/A            | (((S((SK)K))((SK)K))((S((SK)K))((SK)K)))
48|λab.bb                |2| KM    | Konstant Mocker             | KM                   | N/A            | (K((S((SK)K))((SK)K)))
49|λab.aa                |2| C(KM) | Crossed Konstant Mocker     | C(KM)                | N/A            | ((S(K(S(K((S((SK)K))((SK)K))))))K)
50|λ                     |0| Θ     | Theta                       | YO                   | N/A            | ((((SS)K)((S(K((SS)(S((SS)K)))))K)(S((SK)K))))



## Derivations in JS

```js
(
  ( (SS) K)
    ( (S
      (K (
        (SS)
        ( S( (SS) K ) )
      ))
    ) K)
)
```
