''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Muj maly pokus o vytvoren¡ vlastn¡ho Dose, nakonec jsem se daleko nedostal '
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
CLS
5
FILES
PRINT "Vytvor podadresar...1"
PRINT "Zrus podadresar...2"
PRINT "Zmen podadresar...3"
PRINT "Vymaz soubor...4"
PRINT "Otevri soubor...7"
PRINT "Vytvor a Edituj soubor...8"
PRINT "Edituj soubor...9"
INPUT a
IF a = 1 THEN GOSUB 1
IF a = 2 THEN GOSUB 2
IF a = 3 THEN GOSUB 3
IF a = 4 THEN GOSUB 4
IF a = 7 THEN GOSUB 7
IF a = 8 THEN GOSUB 8
END

1
 INPUT b$
 IF b$ = "dir" THEN GOSUB 6
 MKDIR b$
RETURN 5

2
INPUT b$
 IF b$ = "dir" THEN GOSUB 6
 RMDIR b$
RETURN 5

3
CLS
CHDIR "\"
FOR a = 1 TO 1000
86
INPUT aa$
IF aa$ = "" THEN GOTO 67
IF aa$ = "dir" THEN FILES: GOTO 86
bb$ = bb$ + "\" + aa$
CHDIR bb$
NEXT a
67
RETURN 5

4
FILES
PRINT "Jeden soubor"
PRINT "Jen s priponou"
PRINT "Jen s zacatkem"
PRINT "Vse"
INPUT b
IF b = 1 THEN INPUT "Cele jmeno"; f$: KILL f$
IF b = 2 THEN INPUT "Pripona"; f$: KILL "*.f$"
IF b = 3 THEN INPUT "Zacatek"; f$: LET f$ = f$ + ".*": KILL f$
IF b = 4 THEN KILL "*.*"
RETURN 5

6
FILES
INPUT b$
RETURN

7
INPUT b$
IF b$ = "dir" THEN GOSUB 6
IF RIGHT$(b$, 4) <> ".exe" THEN b$ = b$ + ".exe"
CHAIN b$
RETURN 5

8
CLS
FOR a = 1 TO 10000
INPUT b$
IF b$ = "" THEN GOTO 99
c$ = c$ + CHR$(13) + b$
NEXT a
99
INPUT "jmeno souboru"; o$
ON ERROR GOTO 68
OPEN o$ FOR OUTPUT AS #1
PRINT #1, c$
RETURN

9
INPUT "jmeno souboru"; b$
OPEN b$ FOR APPEND AS #1
FOR a = 1 TO 10000
INPUT a$
IF a$ = "" THEN GOTO 68
PRINT #1, a$
NEXT a
68
RETURN

