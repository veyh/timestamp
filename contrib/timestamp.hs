-- timestamp.hs
--
--     $ ghc -Wall -O -dynamic --make timestamp.hs
--     $ strip timestamp
--
-- Copyright (C) 2011, Adam Sjøgren <asjo@koldfront.dk>. Released under the GPLv3+.

import System.Time
import System.Locale

main :: IO ()
main = do contents <- getContents
          mapM_ timestampLn (lines contents)

timestampLn :: String -> IO ()
timestampLn l = do clocktime <- getClockTime
                   calendartime <- toCalendarTime clocktime
                   putStrLn $ formatCalendarTime defaultTimeLocale "%Y-%m-%d %H:%M:%S\t" calendartime ++ l
