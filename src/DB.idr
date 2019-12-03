module Main

import Control.ST.ImplicitCall
import Effect.Exception
import Control.ST.File as F
import Control.ST
import Prelude.File
import Data.String

dbDir : String
dbDir = "/usr/local/lib/idr-db"

dbFile : String
dbFile = dbDir ++ "/db.txt"

undoLog : String
undoLog = dbDir ++ "/undo.log"

redoLog : String
redoLog = dbDir ++ "/redo.log"

-- Write lock for the DB
data DBSt = Locked | Unlocked

unSpace : String -> String
unSpace x = pack $ filter (\c => not $ isSpace c) (unpack x)


-- Our Key is simply a string without spaces
data Key : Type where
  K : (x : String) -> {auto p : unSpace x = x} -> Key

rowToString : (Key, String) -> String
rowToString (K s, entry) = s ++ " " ++ entry

DbResp : Type
DbResp = Either FileError ()


interface Database (m : Type -> Type) where
  DB : DBSt -> Type

  -- Boot and create/reconnect to the database directory.
  -- starts in a locked state
  start : ST m (Either FileError Var) [addIfRight (DB Unlocked)]

  -- See if locks can be done completely at the type level.
  -- unlock : (db : Var) -> ST m () [db ::: DB Locked :-> DB Unlocked]
  -- lock : (db : Var) -> ST m () [db ::: DB Unlocked :-> DB Locked]

  -- reads can happen regardless of database status, maybe?
  read : (db : Var) -> Key -> ST m (Either FileError (Maybe String)) [db ::: DB s]

  -- Transactional write
  prepareWrite : (db : Var) -> (Key, String) -> ST m DbResp [db ::: DB Unlocked :-> DB Locked]

  commitWrite : (db : Var) -> Key -> ST m DbResp [db ::: DB Locked :-> (DB Locked `or` DB Unlocked)]

  abortWrite : (db : Var) -> Key -> ST m DbResp [db ::: DB Locked :-> (DB Locked `or` DB Unlocked)]

makeDir : IO (Either FileError ())
makeDir =
  do eitherDir <- dirOpen dbDir
     case eitherDir of
       Left FileNotFound => do createDir dbDir
       Left err => pure $ Left err
       Right _ => pure $ Right ()

-- db file is reversed so looking for the most recent value is quicker
parseDbFile : String -> List (String, String)
parseDbFile dbFile =
  let lined = lines dbFile
  in go lined
    where
      go [] = []
      go (x :: ys) = go ys ++ [map ltrim (span (/= ' ') x)]

test_parseDbFile1 : parseDbFile "k1 foo bar\nk2 bar" = [("k2", "bar"), ("k1", "foo bar")]
test_parseDbFile1 = Refl

findKey : List (String, String) -> Key -> Maybe String
findKey [] x = Nothing
findKey ((k', entry) :: xs) x@(K k) =
  if k == k'
    then Just entry
    else findKey xs x

findKey' : String -> Key -> Maybe String
findKey' dbstr key = findKey (parseDbFile dbstr) key

test_findKey1 : findKey [("h1", "foo"), ("h3", "bar")] (K "h3") = Just "bar"
test_findKey1 = Refl

test_findKey2 : findKey [("h1", "foo"), ("h3", "bar")] (K "h2") = Nothing
test_findKey2 = Refl

appendToFile : String -> (Key, String) -> ST IO (Either FileError ()) []
appendToFile fname entry =
  do Right f <- F.open fname Append               | Left err => pure (Left err)
     Right _ <- F.writeLine f (rowToString entry) | Left err => do F.close f; pure $ Left err
     F.flush f; ok <- F.close f; pure $ Right ok

writeToRedoAndUndoLog : (Key, String) -> ST IO (Either FileError ()) []
writeToRedoAndUndoLog entry = do appendToFile undoLog entry
                                 appendToFile redoLog entry

removeKey : List (String, String) -> String -> List (String, String)
removeKey [] _ = []
removeKey (r@(x, _) :: xs) k = if x == k then xs else r :: removeKey xs k

serializeDB : List (String, String) -> String
serializeDB ls = (foldl f "" (reverse ls))
  where
    f = (\s, (x, y) => s ++ "\n" ++ x ++ " " ++ y)

clearFile : String -> Key -> ST IO (Either FileError ()) []
clearFile fname (K k) =
  do Right dbstr <- F.readFile fname | Left err => do pure $ Left err
     let db = parseDbFile dbstr
     let removedDb = removeKey db k
     if (length db == length removedDb)
       then do pure $ Left FileNotFound
       else do Right f <- F.open fname WriteTruncate | Left err => do pure $ Left err
               Right ok <- F.writeString f (serializeDB removedDb) | Left err => do F.close f; pure $ Left err
               F.close f
               pure $ Right ok

clearUndoAndRedoLog : Key -> ST IO (Either FileError ()) []
clearUndoAndRedoLog key = do clearFile undoLog key
                             clearFile redoLog key

compareLogAndDb : Key -> String -> String -> Bool
compareLogAndDb key logstr dbstr =
  case (findKey' logstr key, findKey' dbstr key) of
    (Just x, Just y) => x == y
    otherwise        => False

implementation Database IO where
  DB _ = State DBSt

  start = do Right ok <- lift makeDir           | Left err => do pure $ Left err
             Right ok <- F.writeFile dbFile  "" | Left err => do pure $ Left err
             Right ok <- F.writeFile undoLog "" | Left err => do pure $ Left err
             Right ok <- F.writeFile redoLog "" | Left err => do pure $ Left err
             db <- new Unlocked
             pure $ Right db

  read db key = do Right dbstr <- F.readFile dbFile  | Left err => do pure $ Left err
                   pure $ Right (findKey' dbstr key)


  prepareWrite db row = do Right ok <- appendToFile dbFile row | Left err => do pure $ Left err
                           writeToRedoAndUndoLog row

  commitWrite db key = do Right dbstr <- F.readFile dbFile   | Left err => do pure $ Left err
                          Right logstr <- F.readFile redoLog | Left err => do pure $ Left err
                          if compareLogAndDb key logstr dbstr
                            then do Right ok <- clearUndoAndRedoLog key | Left err => do pure $ Left err
                                    pure $ Right ok
                            else do pure $ Left FileNotFound

  abortWrite db key = do Right dbstr <- F.readFile dbFile   | Left err => do pure $ Left err
                         Right logstr <- F.readFile redoLog | Left err => do pure $ Left err
                         if compareLogAndDb key logstr dbstr
                           then do Right ok <- clearFile dbFile key | Left err => do pure $ Left err
                                   Right _ <- clearUndoAndRedoLog key | Left err => do pure $ Left err
                                   pure $ Right ok
                           else do pure $ Left FileNotFound
