# System.Process


```hs
-- imported via System.Process
type Pid :: Type
type Pid = System.Posix.Types.CPid

callCommand :: String -> IO ()
callProcess :: FilePath -> [String] -> IO ()
cleanupProcess ::
  (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle) -> IO ()
createProcess ::
  CreateProcess
  -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
getCurrentPid :: IO Pid
getPid :: ProcessHandle -> IO (Maybe Pid)
getProcessExitCode :: ProcessHandle -> IO (Maybe GHC.IO.Exception.ExitCode)

proc :: FilePath -> [String] -> CreateProcess
rawSystem :: String -> [String] -> IO GHC.IO.Exception.ExitCode
readCreateProcess :: CreateProcess -> String -> IO String
readCreateProcessWithExitCode ::
  CreateProcess
  -> String -> IO (GHC.IO.Exception.ExitCode, String, String)
readProcess :: FilePath -> [String] -> String -> IO String
readProcessWithExitCode ::
  FilePath
  -> [String]
  -> String
  -> IO (GHC.IO.Exception.ExitCode, String, String)
runCommand :: String -> IO ProcessHandle
runInteractiveCommand ::
  String -> IO (Handle, Handle, Handle, ProcessHandle)
runInteractiveProcess ::
  FilePath
  -> [String]
  -> Maybe FilePath
  -> Maybe [(String, String)]
  -> IO (Handle, Handle, Handle, ProcessHandle)
runProcess ::
  FilePath
  -> [String]
  -> Maybe FilePath
  -> Maybe [(String, String)]
  -> Maybe Handle
  -> Maybe Handle
  -> Maybe Handle
  -> IO ProcessHandle
shell :: String -> CreateProcess
showCommandForUser :: FilePath -> [String] -> String
spawnCommand :: String -> IO ProcessHandle
spawnProcess :: FilePath -> [String] -> IO ProcessHandle
system :: String -> IO GHC.IO.Exception.ExitCode
terminateProcess :: ProcessHandle -> IO ()
waitForProcess :: ProcessHandle -> IO GHC.IO.Exception.ExitCode
withCreateProcess :: forall a. CreateProcess
  -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a)
  -> IO a

type CmdSpec :: Type
data CmdSpec = ...
CreatePipe :: StdStream
CreateProcess ::
  CmdSpec
  -> Maybe FilePath
  -> Maybe [(String, String)]
  -> StdStream
  -> StdStream
  -> StdStream
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> Maybe System.Posix.Types.GroupID
  -> Maybe System.Posix.Types.UserID
  -> Bool
  -> CreateProcess

type CreateProcess :: Type
data CreateProcess = ...
Inherit :: StdStream
NoStream :: StdStream

type ProcessHandle :: Type
data ProcessHandle = ...
RawCommand :: FilePath -> [String] -> CmdSpec
ShellCommand :: String -> CmdSpec

type StdStream :: Type
data StdStream = ...
UseHandle :: Handle -> StdStream

child_group :: CreateProcess -> Maybe System.Posix.Types.GroupID
child_user :: CreateProcess -> Maybe System.Posix.Types.UserID
close_fds :: CreateProcess -> Bool
cmdspec :: CreateProcess -> CmdSpec
createPipe :: IO (Handle, Handle)
createPipeFd ::
  IO (System.Posix.Internals.FD, System.Posix.Internals.FD)
createProcess_ ::
  String
  -> CreateProcess
  -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
create_group :: CreateProcess -> Bool
create_new_console :: CreateProcess -> Bool
cwd :: CreateProcess -> Maybe FilePath
delegate_ctlc :: CreateProcess -> Bool
detach_console :: CreateProcess -> Bool
env :: CreateProcess -> Maybe [(String, String)]
interruptProcessGroupOf :: ProcessHandle -> IO ()
new_session :: CreateProcess -> Bool
std_err :: CreateProcess -> StdStream
std_in :: CreateProcess -> StdStream
std_out :: CreateProcess -> StdStream
use_process_jobs :: CreateProcess -> Bool
```
