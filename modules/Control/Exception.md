# Control.Exception

```hs
allowInterrupt  :: IO ()
getMaskingState :: IO MaskingState
assert          :: forall a. Bool -> a -> a


bracket         :: forall a b c. IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracketOnError  :: forall a b c. IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket_        :: forall a b c. IO a -> IO b -> IO c -> IO c

catches    :: forall a. IO a -> [Handler a] -> IO a
evaluate   :: forall a. a -> IO a
finally    :: forall a b. IO a -> IO b -> IO a

mask          :: forall b. ((forall a. IO a -> IO a) -> IO b) -> IO b
interruptible :: forall a. IO a -> IO a
mask_         :: forall a. IO a -> IO a
ioError       :: forall a. IOError -> IO a
onException   :: forall a b. IO a -> IO b -> IO a
mapException  :: forall e1 e2 a. (Exception e1, Exception e2) => (e1 -> e2) -> a -> a

asyncExceptionToException   :: forall e. Exception e => e -> SomeException
asyncExceptionFromException :: forall e. Exception e => SomeException -> Maybe e

catch      :: forall e a.   Exception e => IO a -> (e -> IO a) -> IO a
handle     :: forall e a.   Exception e => (e -> IO a) -> IO a -> IO a
catchJust  :: forall e b a. Exception e => (e -> Maybe b) -> IO a -> (b -> IO a) -> IO a
handleJust :: forall e b a. Exception e => (e -> Maybe b) -> (b -> IO a) -> IO a -> IO a
throw      :: forall a e.   Exception e => e -> a
throwIO    :: forall e   a. Exception e => e -> IO a
throwTo    :: forall e.     Exception e => ThreadId -> e -> IO ()
try        :: forall e   a. Exception e => IO a -> IO (Either e a)
tryJust    :: forall e b a. Exception e => (e -> Maybe b) -> IO a -> IO (Either b a)

uninterruptibleMask_ :: forall a. IO a -> IO a
uninterruptibleMask  :: forall b. ((forall a. IO a -> IO a) -> IO b) -> IO b

class (Typeable e, Show e) => Exception e where
    displayException  :: e -> String
    toException       :: e -> SomeException
    fromException     :: SomeException -> Maybe e

type SomeException :: Type
data SomeException = forall e. Exception e => SomeException e

type ArithException :: Type
data ArithException
  = Overflow
  | Underflow
  | LossOfPrecision
  | DivideByZero
  | Denormal
  | RatioZeroDenominator

type ArrayException :: Type
data ArrayException
  = IndexOutOfBounds String
  | UndefinedElement String

type Handler :: Type -> Type
data Handler a = forall e. Exception e => Handler (e -> IO a)

type AllocationLimitExceeded :: Type
data AllocationLimitExceeded = AllocationLimitExceeded

type AssertionFailed :: Type
newtype AssertionFailed = AssertionFailed String

type AsyncException :: Type
data AsyncException
  = StackOverflow
  | HeapOverflow
  | ThreadKilled
  | UserInterrupt

type BlockedIndefinitelyOnMVar :: Type
data BlockedIndefinitelyOnMVar = BlockedIndefinitelyOnMVar

type BlockedIndefinitelyOnSTM :: Type
data BlockedIndefinitelyOnSTM = BlockedIndefinitelyOnSTM

type CompactionFailed :: Type
newtype CompactionFailed = CompactionFailed String

type Deadlock :: Type
data Deadlock = Deadlock

pattern ErrorCall :: String -> ErrorCall
type ErrorCall :: Type

data ErrorCall = ErrorCallWithLocation String String
type Exception :: Type -> Constraint


type IOException :: Type
data IOException = IOError
  { GHC.IO.Exception.ioe_handle       :: Maybe GHC.IO.Handle.Types.Handle
  , GHC.IO.Exception.ioe_type         :: GHC.IO.Exception.IOErrorType
  , GHC.IO.Exception.ioe_location     :: String
  , GHC.IO.Exception.ioe_description  :: String
  , GHC.IO.Exception.ioe_errno        :: Maybe Foreign.C.Types.CInt
  , GHC.IO.Exception.ioe_filename     :: Maybe FilePath
  }

type MaskingState :: Type
data MaskingState
  = Unmasked
  | MaskedInterruptible
  | MaskedUninterruptible

type NestedAtomically :: Type
data NestedAtomically = NestedAtomically

type NonTermination :: Type
data NonTermination = NonTermination

type SomeAsyncException :: Type
data SomeAsyncException = forall e. Exception e => SomeAsyncException e

type NoMethodError :: Type
newtype NoMethodError = NoMethodError String

type PatternMatchFail :: Type
newtype PatternMatchFail = PatternMatchFail String

type RecConError :: Type
newtype RecConError = RecConError String

type RecSelError :: Type
newtype RecSelError = RecSelError String

type RecUpdError :: Type
newtype RecUpdError = RecUpdError String

type TypeError :: Type
newtype TypeError = TypeError String
```
