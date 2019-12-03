-- http://courses.cs.vt.edu/~cs5204/fall00/distributedDBMS/duckett/tpcp.html

module Main

import Server
import Control.ST
import SessionSockets
import Prelude.File
import TPC


main : IO ()
main = do printLn "hello"
