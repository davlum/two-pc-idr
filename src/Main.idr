module Main

import Data.String
import Data.Vect
import Data.List
import CFFI
import Control.ST
import Control.ST.ImplicitCall
import Control.ST.File
import Pruviloj
import Pruviloj.Derive.DecEq


%link C "pq-idr.o"
%include C  "pq-idr.h"
%lib C "pq"

%dynamic "pq-idr.so"

data ResultPtr = MkResultPtr Ptr

data DbPtr = MkDbPtr Ptr

dbPtrInjective : {x, y: Ptr} -> (MkDbPtr x = MkDbPtr y) -> x = y
dbPtrInjective Refl = Refl

implementation DecEq DbPtr where
  decEq (MkDbPtr ptr1) (MkDbPtr ptr2) with (decEq ptr1 ptr2)
    | Yes p = Yes $ cong p
    | No p  = No $ \h : MkDbPtr ptr1 = MkDbPtr ptr2 => p $ dbPtrInjective h

record DbConn where
  constructor MkDbConn
  user, password, hostname : String
  port : Int
  dbname : String
  
breakMaybe : (Char -> Bool) -> String -> Maybe (String, String)
breakMaybe f s = 
  let (before, after) = break f s
  in case before of
      "" => Nothing
      _  => Just (before, after) 


psqlUriPrefix : String
psqlUriPrefix = "postgresql://"

serializeDbConn : DbConn -> String
serializeDbConn (MkDbConn usr pass host p db) = 
  psqlUriPrefix ++ usr ++ ":" ++ pass ++ "@" ++ host ++ ":" ++ show p ++ "/" ++ db


data Query = Commit | Rollback | Prepare String | Stop

implementation Show Query where
  show Commit   = "COMMIT PREPARED 'foobar';"
  show Rollback = "ROLLBACK PREPARED 'foobar';"
  show (Prepare q) = "BEGIN; PREPARE TRANSACTION 'foobar';\n" ++ q

pqConnectDb : DbConn -> IO DbPtr
pqConnectDb dbConn = do 
    ptr <- foreign FFI_C "connect" (String -> IO Ptr) (serializeDbConn dbConn)
    pure $ MkDbPtr ptr 

pqErrorMessage : DbPtr -> IO String
pqErrorMessage (MkDbPtr p) = foreign FFI_C "dbErrorMessage" (Ptr -> IO String) p

pqResultErrorMessage : ResultPtr -> IO String
pqResultErrorMessage (MkResultPtr p) = foreign FFI_C "resultErrorMessage" (Ptr -> IO String) p


-- Closes the connection to the server. Also frees memory used by the PGconn object.
pqFinish : DbPtr -> IO ()
pqFinish (MkDbPtr p) = foreign FFI_C "dbClose" (Ptr -> IO ()) p

pqExec : DbPtr -> Query -> IO ResultPtr
pqExec (MkDbPtr p) q = do
  ptr <- foreign FFI_C "exec" (Ptr -> String -> IO Ptr) p (show q)
  pure $ (MkResultPtr ptr)

pqClear : ResultPtr -> IO ()
pqClear (MkResultPtr p) = foreign FFI_C "clearResult" (Ptr -> IO ()) p


data ConnectionStatus
  = Ok
  | Bad
  | Started
  | Made
  | AwaitingResponse
  | AuthOk
  | Setenv
  | SslStartup
  | Needed
  | OtherConn Int

total
decConnectionStatusEq : (x, y : ConnectionStatus) -> Dec (x = y)
%runElab (deriveDecEq `{decConnectionStatusEq})

implementation DecEq ConnectionStatus where
  decEq x y = decConnectionStatusEq x y

data DbError : Type where
  ConnErr : ConnectionStatus -> String -> DbError
  FileErr : FileError -> DbError
  UriParseError : String -> DbError

parseDbConn : String -> Maybe DbConn
parseDbConn str = 
  if isPrefixOf psqlUriPrefix str
  then do
    let rest = pack $ drop (length psqlUriPrefix) (unpack str)
    (usr, rest) <- breakMaybe (== ':') rest
    (pass, rest) <- breakMaybe (== '@') (strTail rest)
    (host, rest) <- breakMaybe (== ':') (strTail rest)
    (portStr, rest) <- breakMaybe (== '/') (strTail rest)
    port <- parsePositive portStr
    let db = strTail rest
    pure $ MkDbConn usr pass host port db
  else Nothing

parseDbConn' : String -> Either DbError DbConn
parseDbConn' str = case parseDbConn str of
                        Just c  => Right c
                        Nothing => Left (UriParseError str)

decFileErrorEq : (x, y : FileError) -> Dec (x = y)
%runElab (deriveDecEq `{decFileErrorEq})

implementation DecEq FileError where
  decEq x y = decFileErrorEq x y

total
decDbErrorEq : (x, y : DbError) -> Dec (x = y)
%runElab (deriveDecEq `{decDbErrorEq})

implementation DecEq DbError where
  decEq x y = decDbErrorEq x y


connErrNotFileErr : (ConnErr x y = FileErr z) -> Void
connErrNotFileErr Refl impossible

connErrNotUriParseError : (ConnErr x y = UriParseError s) -> Void
connErrNotUriParseError Refl impossible

fileErrNotUriParseError : (FileErr f = UriParseError s) -> Void
fileErrNotUriParseError Refl impossible


mkConnectionStatus: Int -> ConnectionStatus
mkConnectionStatus i = case i of
  0 => Ok
  1 => Bad
  2 => Started
  3 => Made
  4 => AwaitingResponse
  5 => AuthOk
  6 => Setenv
  7 => SslStartup
  8 => Needed
  _ => OtherConn i

pqStatus : DbPtr -> IO ConnectionStatus
pqStatus (MkDbPtr p) = do
  statusCode <- foreign FFI_C "PQstatus" (Ptr -> IO Int) p
  pure $ mkConnectionStatus statusCode


getConnection : DbConn -> IO (Either DbError DbPtr)
getConnection dbConn = do
  dbPtr <- pqConnectDb dbConn
  connStatus <- pqStatus dbPtr
  case connStatus of
    Ok => pure (Right dbPtr)
    otherStatus => do
      msg <- pqErrorMessage dbPtr
      pqFinish dbPtr
      pure $ Left (ConnErr otherStatus msg)

Show ConnectionStatus where
  show x = case x of
    Ok               => "OK"
    Bad              => "BAD"
    Started          => "STARTED"
    Made             => "MADE"
    AwaitingResponse => "AWAITING_RESPONSE"
    AuthOk           => "AUTH_OK"
    Setenv           => "SETENV"
    SslStartup       => "SSL_STARTUP"
    Needed           => "NEEDED"
    OtherConn i      => "OTHER " ++ show i

Show DbError where
  show (ConnErr status msg) = show status ++ ": " ++ msg
  show (FileErr err) = show err
  show (UriParseError input) = "Unable to parse `" ++ input ++ "` as uri."


data ResultStatus
  = EmptyQuery
  | CommandOk
  | TuplesOk
  | CopyOut
  | CopyIn
  | BadResponse
  | NonFatalError
  | FatalError
  | CopyBoth
  | SingleTuple
  | OtherRes Int

data ResultError = MkResultError ResultStatus String

mkResultStatus : Int -> ResultStatus
mkResultStatus i = case i of
  0 => EmptyQuery
  1 => CommandOk
  2 => TuplesOk
  3 => CopyOut
  4 => CopyIn
  5 => BadResponse
  6 => NonFatalError
  7 => CopyBoth
  8 => SingleTuple
  i => OtherRes i

pqResultStatus : ResultPtr -> IO ResultStatus
pqResultStatus (MkResultPtr p) = do 
  statusCode <- foreign FFI_C "PQresultStatus" (Ptr -> IO Int) p
  pure $ mkResultStatus statusCode

Show ResultStatus where
  show x = case x of
    EmptyQuery    => "EmptyQuery"
    CommandOk     => "CommandOk"
    TuplesOk      => "TuplesOk "
    CopyOut       => "CopyOut"
    CopyIn        => "CopyIn"
    BadResponse   => "BadResponse"
    NonFatalError => "NonFatalError"
    FatalError    => "FatalError"
    CopyBoth      => "CopyBoth"
    SingleTuple   => "SingleTuple"
    OtherRes i       => "Unknown exit code: " ++ show i

implementation Show ResultError where
  show (MkResultError code msg) = "ResultError: Code " ++ show code ++ ": Msg:" ++ msg

execQuery : DbPtr -> Query -> IO (Either ResultError ResultStatus)
execQuery dbPtr query = do
  resPtr <- pqExec dbPtr query
  resultStatus <- pqResultStatus resPtr
  errMsg <- pqResultErrorMessage resPtr
  pqClear resPtr
  case errMsg of
    "" => pure $ Right resultStatus
    msg => pure $ Left (MkResultError resultStatus msg)


interface Database (m : Type -> Type) where
  Db : Nat -> Type

  connect : DbConn -> ST m (Either DbError Var) [addIfRight (Db 1)]
  disconnect : (db : Var) -> ST m () [remove db (Db 1)]
  query : (db : Var) -> Query -> ST m (Either ResultError ResultStatus) [db ::: Db 1]
  
  connectMany : Vect n DbConn -> ST m (Either (List DbError) Var) [addIfRight (Db n)]
  disconnectMany : (db: Var) -> ST m () [remove db (Db n)]
  queryMany : (db : Var) -> Query -> ST m (Either (List ResultError) (Vect n ResultStatus)) [db ::: Db n]


getRight : Either a b -> Maybe b
getRight e = case e of
                  Right x => Just x
                  Left _  => Nothing

aggErrors : Vect n (Either a b) -> Either (List a, List b) (Vect n b)
aggErrors {n} eithers = let (MkDPair len vec) = mapMaybe getRight eithers
                        in case exactLength n vec of
                                Just v  => Right v
                                Nothing => Left (partitionEithers $ toList eithers)
                                           

implementation Database IO where
  Db n = State (Vect n DbPtr)

  connect dbConn = do
    eitherPtr <- lift $ getConnection dbConn
    case eitherPtr of
      Right ptr => do var <- new (ptr :: Vect.Nil)
                      pure $ Right var
      Left err => pure $ Left err

  disconnect db = do
    dbPtr <- read db
    lift $ pqFinish (head dbPtr)
    delete db

  query db query = do
    dbPtr <- read db
    lift $ execQuery (head dbPtr) query

  connectMany conns = do
    eithers <- lift $ traverse getConnection conns
    case aggErrors eithers of
         Right vec => do var <- new vec
                         pure $ Right var
         Left (errs, ptrs) => do lift $ traverse pqFinish ptrs
                                 pure (Left errs)

  disconnectMany db = do
    dbPtrs <- read db
    lift $ traverse pqFinish dbPtrs
    delete db

  queryMany db query = do
    dbPtrs <- read db
    eithers <- lift $ traverse (\dbPtr => execQuery dbPtr query) dbPtrs
    case aggErrors eithers of
         Right vec      => pure $ Right vec
         Left (errs, _) => pure $ Left errs

  

------- End Database Module -------
------- Start Two PC Module -------

data CoordState = Init | Abort | Persist

interface Coord (m : Type -> Type) where
  MkCoord : Nat -> CoordState -> Type

  connectPeers : Vect n DbConn -> ST m (Either (List DbError) Var) [addIfRight (MkCoord n Init)]
  disconnectPeers : (coord : Var) -> ST m () [remove coord (MkCoord n Init)]

  prepare : (coord : Var) -> Query -> ST m (Either (List ResultError) ()) [coord ::: MkCoord n Init :-> 
                                                                            (\res => MkCoord n (case res of 
                                                                                                  Right _ => Persist
                                                                                                  Left  _ => Abort))]

  persist : (coord : Var) -> ST m (Either (List ResultError) ()) [coord ::: MkCoord n Persist :-> 
                                                                   (\res => MkCoord n (case res of 
                                                                                         Right _ => Init
                                                                                         Left  _ => Persist))]

  abort : (coord : Var) -> ST m (Either (List ResultError) ()) [coord ::: MkCoord n Abort :->
                                                                 (\res => MkCoord n (case res of 
                                                                                         Right _ => Init
                                                                                         Left  _ => Abort))]


Database m => Coord m where 
  MkCoord n x = Db n {m}

  connectPeers connVec = do 
    Right db <- connectMany connVec | Left errs => pure (Left errs)
    pure (Right db)

  disconnectPeers coord = do
    disconnectMany coord

  prepare coord query = do
    Right _ <- queryMany coord query | Left errs => pure (Left errs)
    pure (Right ())

  persist coord = do
    Right _ <- queryMany coord Commit | Left errs => pure (Left errs)
    pure (Right ())

  abort coord = do
    Right _ <- queryMany coord Rollback | Left errs => pure (Left errs)
    pure (Right ())


retryAbort : (ConsoleIO m, Coord m) => (coord : Var) -> ST m () [coord ::: MkCoord n {m} Abort :-> MkCoord n {m} Init]
retryAbort coord = do
  status <- abort coord
  case status of
       Right _   => putStrLn "Abort succeeded."
       Left errs => do putStrLn ("Abort failed:\n" ++ unlines (map show errs))
                       retryAbort coord


retryPersist : (ConsoleIO m, Coord m) => (coord : Var) -> ST m () [coord ::: MkCoord n {m} Persist :-> MkCoord n {m} Init]
retryPersist coord = do
  status <- persist coord
  case status of
       Right _   => putStrLn "Commit succeeded."
       Left errs => do putStrLn ("Persist failed:\n" ++ unlines (map show errs))
                       retryPersist coord


transact : (ConsoleIO m, Coord m) => (coord : Var) -> Query -> ST m () [coord ::: MkCoord n {m} Init]
transact coord query = do
  status <- prepare coord query
  case status of
       Right _   => do putStrLn "Prepare succeeded. Commiting transaction..."
                       retryPersist coord
       Left errs => do putStrLn ("Prepare failed:\n" ++ unlines (map show errs))
                       retryAbort coord


getString : ConsoleIO m => ST m String []
getString = go "" where
  go str = do
    line <- getStr
    case line of
      "" => pure str
      ln => go $ str ++ ln


getQuery : ConsoleIO m => ST m Query []
getQuery = do
  maybeQuery <- getString
  case maybeQuery of 
       "END" => pure Stop
       query => pure (Prepare query)


transactQueries : (ConsoleIO m, Coord m) => (coord : Var) -> ST m () [coord ::: MkCoord n {m} Init]
transactQueries coord = do
  putStrLn "Input query or END to Stop:" 
  maybeQuery <- getQuery
  case maybeQuery of
       Stop => pure ()
       query => do transact coord query
                   transactQueries coord
        

session : (ConsoleIO m, Coord m) => Vect n DbConn -> ST m () []
session conns = do
  Right coord <- connectPeers conns | Left errs => putStrLn ("Connection failed:\n" ++ unlines (map show errs))
  putStrLn "Connected to peers."
  transactQueries coord
  disconnectPeers coord


fromListOfLength : (n : Nat) -> (xs : List a) -> Maybe (Vect n a)
fromListOfLength n xs with (decEq (length xs) n)
  fromListOfLength n xs | (Yes prf) = rewrite (sym prf) in Just (fromList xs)
  fromListOfLength n xs | (No _) = Nothing


parseConnectionFile : File m => String -> ST m (Either DbError (List DbConn)) []
parseConnectionFile path = do
  Right str <- readFile path | Left err => pure (Left $ FileErr err)
  let eitherConns = traverse parseDbConn' (map trim $ lines str)
  case eitherConns of
    Left err => pure (Left err)
    Right conns => pure (Right conns)


main : IO ()
main = do
  args <- getArgs
  case fromListOfLength 2 args of
    Nothing => putStrLn $ "Bad Usage: " ++ unwords args ++ "\nUsage: ./<executable> path-to-config"
    Just vec => do 
      Right connLs <- run (parseConnectionFile (last vec)) | Left err => putStrLn (show err)
      case fromListOfLength (length connLs) connLs of
           Nothing => ?nothing
           Just vec => run (session vec)
     
