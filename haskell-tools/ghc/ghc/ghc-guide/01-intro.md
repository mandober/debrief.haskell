# Introduction

1. Introduction
  - [Obtaining GHC](#obtaining-ghc)
[Meta-information](#meta-info)
  - [Reporting bugs in GHC](#reporting-bugs-in-ghc)
  - [GHC version numbering policy](#ghc-version-numbering-policy)



This is a guide to using GHC: *an interactive and batch compilation system* for the Haskell 2010 language.

Seemingly, GHC has two main components:
- interactive Haskell interpreter, GHCi
- batch compiler, GHC

But in fact, GHC consists of a single program which is just run with different options to provide either the *interactive* or the *batch system*.

The batch compiler can be used alongside GHCi: compiled modules can be loaded into an interactive session and used in the same way as interpreted code, and in fact when using GHCi most of the library code will be pre-compiled. This means you get the best of both worlds: fast pre-compiled library code, and fast compile turnaround for the parts of your program being actively developed.

GHC supports numerous language extensions.

GHC has a comprehensive optimiser, so GHC can produce pretty fast code. Alternatively, the default option is to compile as fast as possible while not making too much effort to optimise the generated code; however, GHC probably isn't what you'd describe as a fast compiler.

GHC profiling system supports "cost centre stacks": a way of seeing the profile of a Haskell program in a call-graph like structure.

GHC comes with a number of libraries.

## Obtaining GHC

[GHC home page](http://www.haskell.org/ghc/)

To build GHC yourself, head on over to the `GHC Building Guide <building>`{.interpreted-text role="ghc-wiki"} to
find out how to get the sources, and build it on your system. Note that
GHC itself is written in Haskell, so you will still need to install GHC
in order to build it.


## Meta-info

Meta: web sites, mailing lists, etc.

On the World-Wide Web, there are several URLs of likely interest:
- [GHC home page](http://www.haskell.org/ghc/)
- [GHC Devs Home](https://gitlab.haskell.org/ghc/ghc) (dev docs, wiki, bug)

We run the following mailing lists about GHC. We encourage you to join, as you feel is appropriate.

:   This list is for GHC users to chat among themselves. If you have a
    specific question about GHC, please check the
    [FAQ](http://www.haskell.org/haskellwiki/GHC/FAQ) first.

    Subscribers can post to the list by sending their message to
    <glasgow-haskell-users@haskell.org>. Further information can be
    found on the [Mailman
    page](http://www.haskell.org/mailman/listinfo/glasgow-haskell-users).

`ghc-devs`

:   The GHC developers hang out here. If you are working with the GHC
    API or have a question about GHC's implementation, feel free to
    chime in.

    Subscribers can post to the list by sending their message to
    <ghc-devs@haskell.org>. Further information can be found on the
    [Mailman page](http://www.haskell.org/mailman/listinfo/ghc-devs).

There are several other Haskell and GHC-related mailing lists served by
`www.haskell.org`. Go to <http://www.haskell.org/mailman/listinfo/> for
the full list.


## Reporting bugs in GHC

Glasgow Haskell is a changing system so there are sure to be bugs in it.
If you find one, please see
`this wiki page <report-a-bug>`{.interpreted-text role="ghc-wiki"} for
information on how to report it.


## GHC version numbering policy

As of GHC 6.8, we have adopted this policy for numbering GHC versions:

* Stable branches are numbered `x.y`, where ⟨y⟩ is *even*. Releases on the stable branch `x.y` are numbered `x.y.z`, where ⟨z⟩ >= 1 is the patchlevel number. Patchlevels are bug-fix releases only, and never change the programmer interface to any system-supplied code. However, if you install a new patchlevel over an old one you will need to recompile any code that was compiled against the old libraries.

* The value of `__GLASGOW_HASKELL__` for a major release `x.y.z` is the integer ⟨xyy⟩; if ⟨y⟩ is a single digit, then a leading zero is added, so for example in version 6.8.2 of GHC we would have `__GLASGOW_HASKELL__==608`.

* We may make snapshot releases of the current stable branch [available for download](http://www.haskell.org/ghc/dist/stable/dist/), and the latest sources are available from the git repositories repositories.

* Stable snapshot releases are named `x.y.z.YYYYMMDD`. where `YYYYMMDD` is the date of the sources from which the snapshot was built, and  `x.y.z+1` is the next release to be made on that branch. For example, `6.8.1.20040225` would be a snapshot of the `6.8` branch during the development of `6.8.2`.

* We may make snapshot releases of the HEAD [available for download](http://www.haskell.org/ghc/dist/current/dist/), and the latest sources are available from the git repositories repositories.

* Unstable snapshot releases are named `x.y.YYYYMMDD`. where `YYYYMMDD` is the date of the sources from which the snapshot was built. For example, `6.7.20040225` would be a snapshot of the HEAD before the creation of the `6.8` branch.

* The value of `__GLASGOW_HASKELL__` for a snapshot release is the integer ⟨xyy⟩. You should never write any conditional code which tests for this value, however: since interfaces change on a day-to-day basis, and we don't have finer granularity in the values of `__GLASGOW_HASKELL__`, you should only conditionally compile using predicates which test whether `__GLASGOW_HASKELL__` is equal to, later than, or earlier than a given major release.

* The version number of your copy of GHC can be found by invoking `ghc` with the `--version` flag.

* The compiler version can be tested within compiled code with the `MIN_VERSION_GLASGOW_HASKELL` CPP macro (defined only when `CPP`is used).
