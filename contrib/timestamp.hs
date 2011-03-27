-- timestamp.hs
--
--     $ ghc -Wall -O -dynamic -o timestamp timestamp.hs
--     $ strip timestamp
--
-- Copyright (C) 2011, Adam Sjøgren <asjo@koldfront.dk>. Released under the GPLv3+.

import System.IO
import System.Time
import System.Locale

main :: IO ()
main = do hSetBuffering stdin LineBuffering
          loop stdin

loop :: Handle -> IO ()
loop h = do eof <- hIsEOF h
            if eof
              then return ()
              else do clocktime <- getClockTime
                      calendartime <- toCalendarTime clocktime
                      string <- hGetLine h
                      putStrLn $ formatCalendarTime defaultTimeLocale "%Y-%m-%d %H:%M:%S\t" calendartime ++ string
                      loop h
