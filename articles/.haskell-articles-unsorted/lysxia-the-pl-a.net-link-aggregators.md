# Lysxia - The pl-a.net link aggregators

> A blog about functional programming

Posted on July 26, 2020

Announcement: two new sites
---------------------------

As a programming language enthusiast, I find lots of interesting news and discussions on a multitude of social media platforms. I made two sites to keep track of everything new online related to Coq and Haskell:

*   **Planet Coq**: [https://coq.pl-a.net](https://coq.pl-a.net/)
*   **Haskell Planetarium**:[1](#fn1) [https://haskell.pl-a.net](https://haskell.pl-a.net/)

If you were familiar with [Haskell News](https://github.com/haskellnews/haskellnews), and missed it since it closed down, Haskell Planetarium is a clone of it.

While the inspiration came from Haskell News, this particular project started with the creation of Planet Coq. Since the Coq community is much smaller, posts and announcements are rarer while also more likely to be relevant to any one member, so there is more value in merging communication channels.

I’m told [“planets”](https://en.wikipedia.org/wiki/Planet_(software)), historically, were more specifically about aggregating blogs of community members. In light of the evolution of social media, it is hopefully not too far-fetched to generalize the word to encompass posts on the various discussion platforms now available to us. Haskell Planetarium includes the blog feed of Planet Haskell; Planet Coq is still missing a blog feed, but that should only be temporary.

The pl-a.net site generator
---------------------------

Under the hood, the link aggregators consist of a special-purpose static site generator, written in OCaml ([source code](https://gitlab.com/Lysxia/coq-planet)). The hope was maybe to write some module of it in Coq, but I didn’t find an obvious candidate with a property worth proving formally. Some of the required libraries, in particular those for parsing (gzip, HTML, JSON, mustache templates), are clearer targets to be rewritten and verified in Coq.

The pl-a.net domain
-------------------

I love pun domains. This one certainly makes me look for new projects related to programming languages (PL) just so that I could host them under that name.

An obvious idea is to spin up new instances of the link aggregator for other programming languages. If someone wants to see that happen, the best way is to clone [the source code](https://gitlab.com/Lysxia/coq-planet) and submit a merge request with a new configuration containing links relevant to your favorite programming language ([guide](https://gitlab.com/lysxia/coq-planet/#create-a-link-aggregator-instance-for-a-new-topic)).

Questions and suggestions about the project are welcome, feel free to [open a new issue on Gitlab](https://gitlab.com/Lysxia/coq-planet/-/issues) or [send me an email](mailto:lysxia@gmail.com).

Other places for comments:

*   [Planet Coq announcement on Discourse](https://coq.discourse.group/t/planet-coq-a-link-aggregator-about-coq/949)
*   [Haskell Planetarium announement on reddit](https://www.reddit.com/r/haskell/comments/hxcypf/haskell_planetarium_link_aggregator_to_haskell/)

* * *

1.  Thus named to not conflict with the already existing [Planet Haskell](https://planet.haskell.org/).[↩︎](#fnref1)


[Source](https://blog.poisson.chat/posts/2020-07-26-pl-anet.html)