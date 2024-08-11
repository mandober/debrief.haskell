# Type Witnesses in Haskell

* Type Witnesses in Haskell - Sandeep Chandrika, 2020
https://serokell.io/blog/haskell-type-level-witness

A **type witness** or **runtime evidence** is a value that in some way holds some type-level information associated with a *polymorphic value* and makes it available to the *type checking* process.

But this is confusing because type checking happens at compile time and values are often available only at runtime. So how can values provide type information at compile time?

It is possible because even though values are only available at runtime, if there is a branch in the code (if-then-else, case statements) that branches on a value, we can make assumptions about that value inside each branch.

For example, if there is a branch in the code

```hs
if i == 1
then {- block 1 -}
else {- block 2 -}
```

we can safely assume that if we find ourselves in block 1, then `i` will be 1 inside that block, and if we find ourselves in block 2, then `i` was not 1.

Thus, at compile time, we'll have some information about a conditional value that branches the code.

The core idea of the type witness technique is to use this information to make the compiler infer the attributes of a polymorphic type, such as what the inferred type is, how it is constrained, etc.

For this, we first need a way to link a value to some type-level code, so consider this GADT, for example

```hs
data MyData a where
  MyValue1 :: MyData Int
  MyValue2 :: MyData String
  MyValue3 :: MyData Char
```

The powerful thing about GADTs is they allow us to write explicit signatures for data ctors, and they can be of a more concrete type than the type we are defining (they are like its subtypes).

We declared 3 data ctors whose types instantiate the type param `a` in `MyData a` in different ways.

>How does this type act as a *witness*?

In the following function, by pattern matching on the `m :: MyData a` value, we can determine what `a` is.

```hs
func1 :: MyData a -> a
func1 m = case m of
  MyValue1 -> 10           -- here `a` is Int
  MyValue2 -> "string"     -- here `a` is String
  MyValue3 -> 'c'          -- here `a` is Char
```

But how is the name "witness" justified? What is it witnessing?
~~the type equality~~

Imagine that this function is part of an expression like

```hs
(10 :: Int) + (func1 MyValue1)
```

The `MyValue1` data ctor, by being part of the call site, is *witnessing* a piece of information that is only available there, i.e. that the type required at the call site is actually `Int`. Hence the name "type witness". 

We can have witnesses that witness other things: equality of types, that `a` has been constrained in a certain way, etc.

## Singletons

Values of `MyData a` can point to what `a` is. In addition, since each polymorphic variant of `MyData a` contains only one value, the concrete types of `MyData a` also determine the value because there is only one possible value for any given variant (?!).

Types with one-to-one correspondence between the type and its value are called *singletons*.

## Phantom types

Static type systems can feel very restrictive at the beginning, but, if they are sufficiently advanced, you will find that you can get some of that flexibility of dynamically typed languages back while retaining the safety of static typing.

Let's see an example where this is manifested, which also involves the use of a type witness. Imagine you are building an application that has users with different privileges. We represent the possible privileges and users like

```hs
data UserPrivilege = Member | Admin | Guest

data User = User
  { userId :: Integer
  , userName :: String
  , userPrivilege :: UserPrivilege
  }
```

Since we are interested in type safety, we want to have `userPrivilege` attribute at type level, so that if we pass a user, that has the privilege `Member`, to a function that requires a user with the privilege `Admin`, the compiler catches it at compile time.

To do this, we add a phantom type to the `User` that acts like a tag.

```hs
data User (u :: UserPrivilege)
   = User { userId :: Integer, userName :: String }
```

However, it is now impossible to write a function that reads a user from the database without explicitly specifying which privilege the user has. So, e.g. if we try to implement this function with the following type

```hs
fetchUserById :: forall a. Int -> IO (User a)
```

it won't be possible because if we get the user from the database and find it has the type 'member', we won't be able to return the concrete type `User 'Member` because the signature says it returns `User a`, for all `a`.

The idea of a polymorphic value `User a` is that we should be able to instantiate it into any type, *as required by the expression where the polymorphic value is used*.

In the `fetchUserById` function, if we find the user in the db to have a privilege of `Admin`, we can return the concrete value only after checking that the caller of this function is indeed asking for `User 'Admin`.

We have seen how it can be done in the `func1` function earlier. But here, we won't be able to use something like that because we wouldn't know the privilege of the user when we make the `fetchUserById` call.

A solution to this problem is to wrap the `user` type in another type, which will have multiple constructors, each wrapping a different type of user, thus hiding the type-level privilege behind them.

```hs
data UserWrapper
  = MemberUser (User 'Member)
  | AdminUser  (User 'Admin)
  | GuestUser  (User 'Guest)
```

The problem with this approach is that you will have to match on all these constructors every time you read a `user` from db to do anything with it, even when you don't care about the privilege of the user.

Another way to hide the type-level privilege is by using a `GADT` wrapper type that hides the type-level privilege behind a GADT constructor.

```hs
data SomeUser where
  SomeUser :: forall a. User a -> SomeUser
```

Since `SomeUser` type ctor does not have the type parameter, we can wrap it around a `User a` of any privilege and return it from the db getter function.

But now, we will find that the `User a` that is unwrapped from the `SomeUser` type can only be used with functions that accept a polymorphic user, that is, `User a`, and cannot be used with a function that requires concrete types, such as `User 'Admin`.

This is exactly what we wanted in the first place. We are prevented from passing a user of unknown privilege to a function that requires a concrete privilege.

But it seems that now we cannot make that call at all. How can we convince the type checker that the `User a` unwrapped from `SomeUser` is in fact, e.g. `User 'Admin`.

We can do that by using a type witness. 
We add the following type to act as a witness.

```hs
data WitnessPrivilege up where
  WitnessMember :: WitnessPrivilege Member
  WitnessGuest  :: WitnessPrivilege Guest
  WitnessAdmin  :: WitnessPrivilege Admin
```

Then we change the `User` type to include this witness as one of its fields.

```hs
data User (u :: UserPrivilege) = User
  { userId :: Integer
  , userName :: String
  , userPrivilege :: WitnessPrivilege u
  }
```

And that is it.

When you want to convert a `User a` unwrapped from `SomeUser` to a concrete type, like `User 'Admin`, you only have to pattern match on the `userPrivilege` field. As soon as you get a match on the `WitnessAdmin` branch, GHC will have inferred the `User a` to be an `User 'Admin`, and allow you to call functions that require `User 'Admin`.

Thanks to the included type witness, we get the best of both worlds; a type-level user privilege which gets out of the way when you don't need it, but can pop up anytime you need it.


### Full code sample

```hs
{-# Language GADTs #-}
{-# Language DataKinds #-}
{-# Language KindSignatures #-}
{-# Language ExistentialQuantification #-}
{-# Language ScopedTypeVariables #-}

module Main where

import Data.List

-- User privileges for our users
data UserPrivilege = Member | Admin | Guest

-- Our type witness
data WitnessPrivilege up where
  WitnessMember :: WitnessPrivilege Member
  WitnessGuest :: WitnessPrivilege Guest
  WitnessAdmin :: WitnessPrivilege Admin

-- Our user type
data User (up :: UserPrivilege) = User
  { userId :: Integer
  , userName :: String
  , userPrivilege :: WitnessPrivilege up
  }

-- The type that we use to hide the privilege type variable
data SomeUser where
  SomeUser :: User a -> SomeUser

-- A function that accept a user id (Integer), and reads
-- the corresponding user from the database. Note that the return
-- type level privilege is hidden in the return value `SomeUser`.
readUser :: Integer -> IO SomeUser
readUser userId = pure $ case find ((== userId) . (\(a, _, _) -> a)) dbRows of
  Just (id_, name_, type_) ->
    case type_ of
      "member" -> SomeUser (User id_ name_ WitnessMember)
      "guest" -> SomeUser (User id_ name_ WitnessGuest)
      "admin" -> SomeUser (User id_ name_ WitnessAdmin)
  Nothing -> error "User not found"

-- This is a function that does not care
-- about user privilege
getUserName :: User up -> String
getUserName = userName

-- This is a function only allows user
-- with Admin privilege.
deleteStuffAsAdmin :: User 'Admin -> IO ()
deleteStuffAsAdmin _ = pure ()

main :: IO ()
main = do
  (SomeUser user) &lt;- readUser 12

  putStrLn $ getUserName user -- We don't care about user privilege here

  case userPrivilege user of -- But here we do.
    -- So we bring the type-level user privilege in scope by matching
    -- on `userPrivilege` field and then GHC knows that `user`
    -- is actually `User 'Admin`, and so we can call `deleteStuffAsAdmin`
    -- with `user`.
    WitnessAdmin ->
      deleteStuffAsAdmin user
    _ -> error "Need admin user"

dbRows :: [(Integer, String, String)]
dbRows =
  [ (10, "John", "member")
  , (11, "alice", "guest")
  , (12, "bob", "admin")
  ]
```


## Refs

- [Introduction to Singletons series][1]
- [Dependently typed programming with singletons][2]
- [Dimensions and Haskell: Singletons in Action][3]
- [Why Dependent Haskell is the Future of Software Development][4]


[1]: https://blog.jle.im/entry/introduction-to-singletons-1.html
[2]: https://repository.brynmawr.edu/cgi/viewcontent.cgi?article=1009&context=compsci_pubs
[3]: https://serokell.io/blog/dimensions-haskell-singletons
[4]: https://serokell.io/blog/why-dependent-haskell
[5]: https://serokell.io/haskell-developers
