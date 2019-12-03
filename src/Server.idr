module Server

import Network.Socket
import SessionSockets
import Control.ST
import TPC
import Control.ST.ImplicitCall


-- cohortQuery' : (Cohort m, ConsoleIO m, Sockets m) => (cohort : Var) -> (sock : Var) ->
--              ST m () [remove sock (Sock {m} Listening), remove cohort (MkCohort {m} Query)]
-- cohortQuery' cohort sock =
--   do Right new <- accept sock                           | Left err => do close sock; remove sock
--      Right msg <- recv new                              | Left err => do close sock; remove sock; remove new
--      ?whatNow_3
     -- case parseMsg msg of
     --   Nothing => do close new; remove new; cohortQuery' cohort sock
     --   Just msg => do msg <- rcvCommitReq cohort msg
     --                  case msg of
     --                       FUBAR => do Right ok <- send new (show msg) | Left err => do remove new; close sock; remove sock
     --                                   shutDown cohort; close new; remove new; close sock; remove sock
     --                       COMMIT _ => ?whatNow_4


     -- Right ok <- rcv parseMsg msg) | Left err => do remove new; close sock; remove sock
     -- close new; remove new; cohortQuery' sock

echoServer : (ConsoleIO m, Sockets m) => (sock : Var) ->
             ST m () [remove sock (Sock {m} Listening)]
echoServer sock =
  do Right new <- accept sock                  | Left err => do close sock; remove sock
     Right msg <- recv new                     | Left err => do close sock; remove sock; remove new
     Right ok <- send new ("You said " ++ msg) | Left err => do remove new; close sock; remove sock
     close new; remove new; echoServer sock

export
startServer : (Cohort m, ConsoleIO m, Sockets m) => ST m () []
startServer =
  do Right sock <- socket Stream        | Left err => pure ()
     Right ok <- bind sock Nothing 9442 | Left err => remove sock
     Right ok <- listen sock            | Left err => remove sock
     echoServer sock
     -- cohort <- boot
     -- cohortQuery' cohort sock
     -- ?whatNow_3
     -- shutDown cohort
