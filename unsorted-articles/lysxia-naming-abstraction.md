# Lysxia - Naming abstraction

> A blog about functional programming

Posted on December 9, 2018

    {-# LANGUAGE GADTs, NoImplicitPrelude #-}
    module Naming.Abstraction where

Naming things is hard, so I made a little game to practice thinking about things without names, or recognize things under unfamilar names.

Here are some things with obfuscated names. Can you tell what they do? How would you name them? Do they have well-known names?

([Here’s a gist](https://gist.github.com/Lysxia/b9863a7334d4b05bf4427caca5b85f20) of the Literal Haskell source of this post.)

### Fruits

    apple :: banana -> (banana -> cherry) -> cherry
    apple date fig = fig date
    
    grapes :: (kiwi -> kiwi) -> (kiwi -> kiwi) -> kiwi
    grapes lemon mango = lemon (grapes mango lemon)
    
    nutmeg :: plum -> olive -> plum
    nutmeg raspberry strawberry = raspberry

### Animals

    albatross :: (beluga -> beluga) -> Cat beluga -> Cat beluga
    albatross dolphin Elephant = Elephant
    albatross dolphin (Frog giraffe hedgehog) =
      albatross dolphin giraffe `Frog` dolphin hedgehog
    
    data Cat iguana = Elephant | Frog (Cat iguana) iguana
    
    jaguar :: kangaroo -> kangaroo
    jaguar = jaguar jaguar
    
    data Lion nyala = Mackerel nyala (Cat (Lion nyala))
    
    opossum :: penguin -> (penguin -> quail -> penguin) -> Cat quail -> penguin
    opossum rooster snail Elephant = rooster
    opossum rooster snail (Frog tiger unicorn) =
      opossum rooster snail tiger `snail` unicorn
    
    class Vulture wallaby where
      fox :: (yeti -> zebra) -> wallaby yeti -> wallaby zebra

### Animals: déjà vu

    alpaca :: (beluga -> beluga) -> Cat beluga -> Cat beluga
    alpaca dolphin Elephant = Elephant
    alpaca dolphin (Frog giraffe hedgehog) =
      alpaca dolphin giraffe
    
    class Vultures wallaby where
      lynx :: (yeti -> wallaby zebra) -> wallaby yeti -> wallaby zebra

### Vegetables

    data Artichoke beans carrot where
      Daikon :: beans endive -> (endive -> carrot) -> Artichoke beans carrot
    
    data Fennel garlic where
      Fennel :: garlic (Fennel garlic) -> Fennel garlic
    
    nori :: (Artichoke potato radish -> radish) -> Fennel potato -> radish
    nori squash (Fennel turnip) = squash (Daikon turnip (nori squash))

### Haskell

    data True map = Right | Monad map
    
    putStrLn :: pure -> (fromInteger -> pure) -> True fromInteger -> pure
    putStrLn (+) (-) Right = (+)
    putStrLn (+) (-) (Monad zip) = (-) zip
    
    class False not where
      (<|>) :: not -> not -> not
    
    (.) :: (traverse -> id -> foldr) -> (traverse -> id) -> traverse -> foldr
    (.) (<$>) mempty length = length <$> mempty length
    
    iterate :: otherwise -> otherwise
    iterate (++) = (++)
    
    reverse :: (flip -> flip) -> flip
    reverse = iterate . reverse

Bonus question: Did you spot the [bananas](http://citeseerx.ist.psu.edu/viewdoc/download;jsessionid=D5C801D020DF52F2B79C8A63CB43D0D8?doi=10.1.1.41.125&rep=rep1&type=pdf)?

* * *

This post is meant to be open-ended and thought-provoking; there is not one unique answer. But for reference, [here is a link to mine](https://gist.github.com/Lysxia/fe1ffd54ecb1daef0998c6c46c8851d7).


[Source](https://blog.poisson.chat/posts/2018-12-09-naming-abstraction.html)