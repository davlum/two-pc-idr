-- http://courses.cs.vt.edu/~cs5204/fall00/distributedDBMS/duckett/tpcp.html

module Main

import Data.Vect
import Control.ST.ImplicitCall
import Effect.Exception
import Control.ST.File as F
import Control.ST
import SessionSockets
import Network.Socket
import Prelude.File
import Data.String
import DB

numCohorts : Int
numCohorts = 2

public export
data NodeSt = Query | Commit | Abort | Wait | Done

public export
data Msg = FUBAR | COMMIT Int

public export
Show Msg where
  show (COMMIT i) = "COMMIT " ++ show i
  show FUBAR = "FUBAR"

export
parseMsg : String -> Maybe Msg
parseMsg "FUBAR" = Just FUBAR
parseMsg x       = case break (== ' ') x of
  ("COMMIT", y) => let parsedInt = the (Maybe Int) (parsePositive y)
                   in case parsedInt of
                        Just i  => Just $ COMMIT i
                        Nothing => Nothing
  otherwise     => Nothing


export
recvMsg : Msg -> NodeSt
recvMsg (COMMIT _) = Commit
recvMsg FUBAR = Abort

public export
interface Cohort (m : Type -> Type) where
  -- sends and recieves only from coord
  MkCohort : NodeSt -> Type
  boot : ST m Var [add (MkCohort Query)]
  shutDown : (cohort : Var) -> ST m () [remove cohort (MkCohort Abort)]
  {-
    1. If a COMMIT-REQUEST message is received for some transaction t which is unknown at the COHORT
   ( never ran, wiped out by crash, etc ), reply ABORT. Otherwise write the new state of the transaction
   to the UNDO and REDO log in permanent memory. This allows for the old state to be recovered
   ( in event of later abort ) or committed on demand regardless of crashes.
   The read locks of a transaction may be released at this time; however, the write locks are still maintained.
   Now send AGREED to the COORDINATOR.
   -}
  rcvCommitReq : (coh : Var) -> Msg -> ST m Msg [coh ::: MkCohort Query :->
                                                  (\msg => MkCohort (case msg of
                                                                          COMMIT _ => Wait
                                                                          FUBAR => Abort))]
  {-
    2. If an ABORT message is received then kill the transaction, which involves deleting
    the new state if the transaction from the REDO and UNDO log the new state of the transaction
    and restoring any state before the transaction occured.
    3. If a COMMIT message is received then the transaction is either prepared for
     commital or already committed. If it is prepared, perform all the operations necessary
     to update the database and release the remaining locks the transaction possesses.
     If it is already commited, no further action is required. Respond COMMITED to the COORDINATOR.
  -}
  rcvCoordResp : (coh : Var) -> ST m Msg [coh ::: MkCohort Wait :->
                                            (\msg => MkCohort (recvMsg msg))]



--
-- cohortRound : (ConsoleIO m, Cohort m) => String -> ST m () []
-- cohortRound = do cohort <- bootCohort
--                  msg <- rcvCommitReq cohort
--                  case msg of
--                    OK => do writeEntry
--                    FUBAR => ?whatNow_2

implementation Cohort IO where
  MkCohort x = ?Cohort_rhs_1
  boot = ?Cohort_rhs_2
  shutDown cohort = ?Cohort_rhs_3
  rcvCommitReq coh x = ?Cohort_rhs_4
  rcvCoordResp coh = ?Cohort_rhs_5
  
interface Coord (m : Type -> Type) where
  MkCoord : NodeSt -> Type
  bootCoord : ST m Var [add (MkCoord Query)]
  -- 1. The COORDINATOR sends the message to each COHORT. The COORDINATOR is now in the preparing transaction state.
  sendCommitReq : (coo : Var) -> ST m () [coo ::: MkCoord Query :-> MkCoord Wait]
  {-
  2. Now the COORDINATOR waits for responses from each of the COHORTS. If any COHORT responds ABORT
  then the transaction must be aborted, proceed to step 5. If all COHORTS respond AGREED then the
  transaction may be commited, and proceed to step 3. If after some time period all COHORTS do not respond
  the COORDINATOR can either transmit ABORT messages to all COHORTS or transmit COMMIT-REQUEST messages to the
  COHORTS that have not responded. In either case the COORDINATOR will eventually go to state 3 or state 5.
  3. Record in the logs a COMPLETE to indicate the transaction is now completing. Send COMMIT message to each of the COHORTS.
  5. Send the ABORT message to each COHORT.
  -}
  rcvCohortResp : (coo : Var) -> ST m Msg [coo ::: MkCoord Wait :->
                                            (\msg => MkCoord (recvMsg msg))]
  rcvCohortCommit : (coo: Var) -> ST m Msg []
