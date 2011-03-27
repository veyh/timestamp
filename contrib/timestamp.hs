-- timestamp.hs
--
--     $ ghc -Wall -o timestamp timestamp.hs
--
-- Copyright (C) 2011, Adam Sjøgren <asjo@koldfront.dk>. Released under the GPLv3+.

import System.IO
import Data.Time
import System.Locale

main :: IO ()
main = do hSetBuffering stdin (LineBuffering)
          t <- getCurrentTime
          zone <- getTimeZone t
          loop stdin zone

loop :: Handle -> TimeZone -> IO ()
loop h zone = do eof <- hIsEOF h
                 if eof
                   then return ()
                   else do timestamp <- getCurrentTime
                           string <- hGetLine h
                           putStrLn $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S\t" (utcToZonedTime zone timestamp) ++ string
                           loop h zone
