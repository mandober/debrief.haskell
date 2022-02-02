# seq


https://stackoverflow.com/questions/12687392/why-is-seq-bad
https://stackoverflow.com/questions/6872898/what-is-weak-head-normal-form?rq=1
https://stackoverflow.com/questions/7490768/what-are-haskells-strictness-points?rq=1
https://stackoverflow.com/questions/23313291/weak-head-normal-form-and-order-of-evaluation?rq=1


## The seq function

```hs
seq :: a -> b -> b
seq x y = y
```

The `seq` function is very special: it is not a normal language function but it is implemented by the compiler which ensures that the type `a` is evaluated; it is strict in the type `a`, i.e. in its first argument.
