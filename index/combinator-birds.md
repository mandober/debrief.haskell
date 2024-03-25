# Combinator birds

https://wiki.haskell.org/Pointfree
https://www.angelfire.com/tx4/cus/combinator/birds.html


nr | Combinator                  | Symbol |C|A| Notes
---|-----------------------------|--------|-|-|-----------------------
 1 | Bluebird                    | B      |n|3| λabc.a(bc), `(.)`
 2 | Blackbird                   | B₁     | |4| λabcd.a(bcd)
 3 | Bunting                     | B₂     | |5| λabcde.a(bcde)
 4 | Becard                      | B₃     | |4| λabcd.a(b(cd))
 5 | Cardinal                    | C      |n|3| λabc.acb, `flip`
 6 | Dove                        | D      | |4| λabcd.ab(cd)
 7 | Dickcissel                  | D₁     | |5| λabcde.abc(de)
 8 | Dovekies                    | D₂     | |5| λabcde.a(bc)(de)
 9 | Eagle                       | E      | |5| λabcde.ab(cde)
10 | Bald Eagle                  | Ê, E₁  | |6| λabcdefg.a(bcd)(efg)
11 | Finch                       | F      | |3| λabc.cba
12 | Goldfinch                   | G      | |4| λabcd.ad(bc)
13 | Hummingbird                 | H      | |3| λabc.abcb
14 | Idiot Bird                  | I      |n|1| λa.a, `id`
15 | Jay                         | J      | |4| λabcd.ab(adc)
16 | Kestrel                     | K      | |2| λab.a
17 | Lark                        | L      | |2| λab.a(bb)
18 | Mockingbird                 | M      | |1| λa.aa
19 | Double Mockingbird          | M₂     | |2| λab.ab(ab)
20 | Owl                         | O      | |2| λab.b(ab)
21 | Queer Bird                  | Q      | |3| λabc.b(ac)
22 | Quixotic Bird               | Q₁     | |3| λabc.a(cb), λabc.a(bc) is
23 | Quizzical Bird              | Q₂     | |3| λabc.b(ca)
24 | Quirky Bird                 | Q₃     | |3| λabc.c(ab)
25 | Quaky Bird                  | Q₄     | |3| λabc.c(ba)
26 | Robin                       | R      | |3| λabc.bca
27 | Starling                    | S      | |3| λabc.ac(bc)
28 | Thrush                      | T      | |2| λab.ba
29 | Turing                      | U      |f|2| λab.b(aab)
30 | Vireo                       | V      |n|3| λabc.cab, `(,)`
31 | Warbler                     | W      | |2| λab.abb
32 | Converse Warbler            | W¹     | |2| λab.baa
33 | Why Bird (aka Sage Bird)    | Y      |f|1| λa.a(λa)
34 | Zero Bird                   | Z      |f|1|   Y strict
35 | Identity Bird Once Removed  | I*, I₁ |o|2| λab.ab
36 | Warbler Once Removed        | W*, W₁ |o|3| λabc.abcc
37 | Cardinal Once Removed       | C*, C₁ |o|4| λabcd.abdc
38 | Robin Once Removed          | R*, R₁ |o|4| λabcd.acdb
39 | Finch Once Removed          | F*, F₁ |o|4| λabcd.adcb
40 | Vireo Once Removed          | V*, V₁ |o|4| λabcd.acbd
41 | Identity Bird Twice Removed | I**, I²|t|3| λabc.abc
42 | Warbler Twice Removed       | W**, W²|t|4| λabcd.abcdd
43 | Cardinal Twice Removed      | C**, C²|t|5| λabcde.abced
44 | Robin Twice Removed         | R**, R²|t|5| λabcde.abdec
45 | Finch Twice Removed         | F**, F²|t|5| λabcde.abedc
46 | Vireo Twice Removed         | V**, V²|t|5| λabcde.abecd
47 | Kite                        | KI     |s|2| λab.b, `const`
48 | Omega                       | Ω, MM  |s| | 
49 | Konstant Mocker             | KM     |s|2| λab.bb
50 | Crossed Konstant Mocker     | C(KM)  |s|2| λab.aa
51 | Theta                       | Θ, YO  |s| | 
52 | Tits `(.)(.)` aka Dove      |        |a| | λabcd.ab(cd)



- `a` alias
- `f` fixpoint
- `n` normal boids
- `o` once removed
- `t` twice removed
- `s` special
- `e` extra

- 4 Mockers: Konstant Mocker, Crossed Konstant Mocker, MB, Double MB
- 4 Warblers: Warbler, Converse Warbler, Once Removed, Twice Removed
- 3 Vireos: Vireo, Once Removed, Twice Removed

Y := λf.(λx.f(xx))(λx.f(xx))
