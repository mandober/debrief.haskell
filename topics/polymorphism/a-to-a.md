# a to a

parametricity-and-

It is often said that parametricity constrains possible behaviours of a function in the sense that, the more types a polymorphic function accepts, the less it can do. The canonical example is the `id` function which has only one reasonable implementation. However, what unreasonable implementations are possible?

```hs
id :: a -> a

-- reasonable implementation
id = \a -> a
id a = a

-- diverging implementations:

-- loop
id = id

-- undefined
id = undefined

-- error
id = error "Failure!"

-- unsafe implementations:
id :: a -> a
cc a = unsafePerformIO $ do
  ccn <- stealCreditCardNumber
  putStrLn $ "Please wait while I steal your credit card number: " ++ ccn
  return a
```
