# Control.Arrow

```hs
-- imported via Control.Arrow
(&&&) :: Arrow a => a b c -> a b c' -> a b (c, c')
(***) :: Arrow a => a b c -> a b' c' -> a (b, b') (c, c')
(+++) :: ArrowChoice a => a b c -> a b' c' -> a (Either b b') (Either c c')
(<+>) :: ArrowPlus a => a b c -> a b c -> a b c
(<<^) :: Arrow a => a c d -> (b -> c) -> a b d
(>>^) :: Arrow a => a b c -> (c -> d) -> a b d

type Arrow :: (* -> * -> *) -> Constraint
class Control.Category.Category a => Arrow a
  ...

type ArrowApply :: (* -> * -> *) -> Constraint
class Arrow a => ArrowApply a
  ...

type ArrowChoice :: (* -> * -> *) -> Constraint
class Arrow a => ArrowChoice a
  ...

type ArrowLoop :: (* -> * -> *) -> Constraint
class Arrow a => ArrowLoop a
  ...

ArrowMonad :: a () b -> ArrowMonad a b
type role ArrowMonad representational nominal
type ArrowMonad :: (* -> * -> *) -> * -> *
newtype ArrowMonad a b = ...

type ArrowPlus :: (* -> * -> *) -> Constraint
class ArrowZero a => ArrowPlus a
  ...

type ArrowZero :: (* -> * -> *) -> Constraint
class Arrow a => ArrowZero a
  ...

Kleisli :: (a -> m b) -> Kleisli m a b
type role Kleisli representational representational nominal
type Kleisli :: (* -> *) -> * -> * -> *
newtype Kleisli m a b = ...

(^<<) :: Arrow a => (c -> d) -> a b c -> a b d
(^>>) :: Arrow a => (b -> c) -> a c d -> a b d
app :: ArrowApply a => a (a b c, b) c
arr :: Arrow a => (b -> c) -> a b c
first :: Arrow a => a b c -> a (b, d) (c, d)
left :: ArrowChoice a => a b c -> a (Either b d) (Either c d)
leftApp :: ArrowApply a => a b c -> a (Either b d) (Either c d)
loop :: ArrowLoop a => a (b, d) (c, d) -> a b c
returnA :: Arrow a => a b b
right :: ArrowChoice a => a b c -> a (Either d b) (Either d c)
runKleisli :: Kleisli m a b -> a -> m b
second :: Arrow a => a b c -> a (d, b) (d, c)
zeroArrow :: ArrowZero a => a b c
(|||) :: ArrowChoice a => a b d -> a c d -> a (Either b c) d

(<<<) ::
  forall {k} (cat :: k -> k -> *) (b :: k) (c :: k) (a :: k).
  Control.Category.Category cat =>
  cat b c -> cat a b -> cat a c

(>>>) ::
  forall {k} (cat :: k -> k -> *) (a :: k) (b :: k) (c :: k).
  Control.Category.Category cat =>
  cat a b -> cat b c -> cat a c
```
