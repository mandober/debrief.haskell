# Arrows: 1. Introduction

John Hughes defined arrows in "Generalising Monads to Arrows"

## Introduction to Arrows

Reversible effects as inverse arrows

Reversible computing models settings in which all processes can be reversed. 

Applications include low-power computing, quantum computing, and robotics.

It is unclear how to represent side-effects in this setting, because conventional methods need not respect reversibility.

We model reversible effects by adapting `Hughes' arrows` to `dagger arrows` and `inverse arrows`. 

This captures several fundamental reversible effects, including serialization and mutable store computations. 


> **Arrows** are monoids in the category of profunctors.

> **Dagger arrows** are involutive monoids in the category of profunctors.

Inverse arrows satisfy certain additional properties. 

These semantics inform the design of functional reversible programs supporting side-effects.
