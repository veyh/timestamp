-- timestamp.hs
--
--     $ ghc -Wall -o timestamp timestamp.hs
--
-- Copyright (C) 2011, Adam Sjøgren <asjo@koldfront.dk>. Released under the GPLv3+.

import System.IO
import Data.Time

main :: IO ()
main = do hSetBuffering stdin (LineBuffering)
          loop stdin

loop :: Handle -> IO ()
loop h = do eof <- hIsEOF h
            if eof
              then return ()
              else do timestamp <- fmap show getZonedTime
                      string <- hGetLine h
                      putStrLn $ take 19 timestamp ++ "\t" ++ string
                      main
