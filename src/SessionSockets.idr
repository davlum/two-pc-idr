module SessionSockets


import Network.Socket
import Control.ST

public export
data SocketState = Ready | Bound | Listening | Open | Closed

public export
data CloseOK : SocketState -> Type where
  CloseOpen : CloseOK Open
  CloseListening : CloseOK Listening

public export
interface Sockets (m : Type -> Type) where
  Sock : SocketState -> Type
  socket : SocketType -> ST m (Either () Var) [addIfRight (Sock Ready)]
  bind : (sock: Var) -> (addr : Maybe SocketAddress) -> (port: Port) ->
         ST m (Either () ()) [sock ::: Sock Ready :-> (Sock Closed `or` Sock Bound)]
  listen : (sock : Var) -> ST m (Either () ()) [sock ::: Sock Bound :-> (Sock Closed `or` Sock Listening)]
  accept : (sock : Var) -> ST m (Either () Var) [sock ::: Sock Listening, addIfRight (Sock Open)]
  send : (sock : Var) -> String -> ST m (Either () ()) [sock ::: Sock Open :-> (Sock Closed `or` Sock Open)]
  recv : (sock : Var) -> ST m (Either () String) [sock ::: Sock Open :-> (Sock Closed `or` Sock Open)]
  close : (sock : Var) -> {auto prf : CloseOK st} -> ST m () [sock ::: Sock st :-> Sock Closed]
  remove : (sock : Var) -> ST m () [Remove sock (Sock Closed)]
  connect : (sock : Var) -> SocketAddress -> Port -> ST m (Either () ()) [sock ::: Sock Ready :-> (Sock Closed `or` Sock Open)]

export
implementation Sockets IO where
  Sock _ = State Socket
  socket ty = do Right sock <- lift $ Socket.socket AF_INET ty 0 | Left err => pure (Left ())
                 lbl <- new sock
                 pure (Right lbl)
  bind sock addr port = do ok <- lift $ bind !(read sock) addr port
                           if ok /= 0
                              then pure (Left ())
                              else pure (Right ())
  listen sock = do ok <- lift $ listen !(read sock)
                   if ok /= 0
                      then pure (Left ())
                      else pure (Right ())
  accept sock = do Right (conn, addr) <- lift $ accept !(read sock)
                         | Left err => pure (Left ())
                   lbl <- new conn
                   returning (Right lbl) (toEnd lbl)

  connect sock addr port
       = do ok <- lift $ connect !(read sock) addr port
            if ok /= 0
               then pure (Left ())
               else pure (Right ())
  close sock = do lift $ close !(read sock)
                  pure ()
  remove sock = delete sock

  send sock msg = do Right _ <- lift $ send !(read sock) msg
                           | Left _ => pure (Left ())
                     pure (Right ())
  recv sock = do Right (msg, len) <- lift $ recv !(read sock) 1024 -- Yes, yes...
                       | Left _ => pure (Left ())
                 pure (Right msg)
