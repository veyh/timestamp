#!/usr/bin/env rexx
/* timestamp.rexx

   Copyright (C) 2011 Klaus Alexander Seistrup <klaus@seistrup.dk>.
   Released under GPLv3+.
*/

SIGNAL ON ERROR    NAME quit
SIGNAL ON FAILURE  NAME quit
SIGNAL ON HALT     NAME quit
SIGNAL ON NOTREADY NAME quit
SIGNAL ON NOVALUE  NAME quit

tab = D2C(9)

DO FOREVER
   PARSE PULL line
   now = DATE('S')
   SAY SUBSTR(now, 1, 4) || '-' || SUBSTR(now, 5, 2) || '-' || SUBSTR(now, 7, 2),
   || ' ' || TIME('N') || tab || line
END

quit:

/* eof */
