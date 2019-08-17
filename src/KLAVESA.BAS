'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'         Ma uplne prvni hra.Ovladate malym 'a' a musite chytit znak ²      '
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
RANDOMIZE TIMER
CLS
LET e = 1
LET f = 1
LET g = 1
LET h = 1
FOR kon = 1 TO 30
LET g = INT(RND * 20) + 1
LET h = INT(RND * 20) + 1
CONST a = 65
1
LOCATE g, h: PRINT "²"
WHILE INKEY$ <> CHR$(97): LET f = f + 1: SLEEP 1: LOCATE e, f: PRINT " þ ": IF g = e AND h = f THEN PRINT "VYHRAL JSI": END
WEND
LET p = INT(RND * 20) + 1
LET w = INT(RND * 20) + 1
CLS
LOCATE p, w: PRINT "²"
WHILE INKEY$ <> CHR$(97): LET e = e + 1: LOCATE e, f:  PRINT " þ ": SLEEP 1: IF p = e AND w = f THEN PRINT "VYHRAL JSI": END
WEND
LET x = INT(RND * 20) + 1
LET t = INT(RND * 20) + 1
CLS
LOCATE x, t: PRINT "²"
WHILE INKEY$ <> CHR$(97): LET f = f - 1: SLEEP 1: LOCATE e, f:  PRINT " þ ": IF x = e AND t = f THEN PRINT "VYHRAL JSI": END
WEND
LET r = INT(RND * 20) + 1
LET z = INT(RND * 20) + 1
CLS
LOCATE r, z: PRINT "²"
WHILE INKEY$ <> CHR$(97): LET e = e - 1: LOCATE e, f: PRINT " þ ": SLEEP 1: IF r = e AND z = f THEN PRINT "VYHRAL JSI": END
WEND
INPUT Q$
CLS
IF Q$ = "END" THEN END
IF Q$ = "C" THEN GOTO 1
NEXT kon
END

