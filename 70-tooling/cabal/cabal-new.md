# cabal


To create a new project `mkdir hsproj` and cd into it, then `cabal init`. Open the project in editor. Create `/src` folder for source files, and within it a new file `/src/Main.hs` containing:

```hs
module Main where
main :: IO a

module Main where
main :: IO ()
main = putStrLn "Hello from Haskell!"
```

For a module to work as a `main-is` (in cabal's config) target for GHC, it must have a function named `main` and itself be named `Main`.


The main function must be a computation of type `IO a`    
  `main :: IO a`   

where `a` is a generic type param.    
Often the unit type is used, `a = ()`, and then the type sig is:    
  `main :: IO ()`

Executing the program calls the `main` function.

The `main` function in a non-trivial program is often contained within a wrapper module called `Main`, which only exists to satisfy this purpose. Sometimes shell arg parsing is done via lib like *optparse-applicative*.

For this demo project we have only one simple module, making it just a putStrLn. To validate that everything is working, let's build it and run it. We're going to create a *Cabal sandbox* so that our dependencies are isolated to this project: 
```
$ cabal sandbox init
```

Now that our sandbox has been created, we want to install our dependencies into the Cabal sandbox package-db:
```
$ cabal install --only-dependencies
```

If you install the dependencies before setting up the sandbox, they would be installed into the user `package-db` located in your home directory, which would be global to all the projects on your current user account. Installing dependencies can take some time on first run.

Now we’re going to build our project:
```
$ cabal build
```

If successful, we should get a binary in dist/build/<proj> 

To run it:
```
$ ./dist/build/hello-haskell/hello-haskell
Hello from Haskell!
```


You can also use `cabal repl` to open a REPL and call main like this:
```
$ cabal repl
*Main> main
Hello from Haskell!
```

Note that using `cabal repl` will compile the module as part of opening the REPL, and you can use the same commands you use in your GHCi, such as `:t` to get type information and `:q` to quit.

If everything is in place, let's move on to factoring out some code and
making a separate module that can be exposed as a library.
