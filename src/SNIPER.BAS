DEFINT A-Z
PRINT "V pripade nefunkcnosti programu zmente adresar"
PRINT "Na pocest nasi (oblibene) profesorky chemie"
SLEEP
DIM pictr(30000)
DIM Prefix(4095), Suffix(4095), OutStack(4095), shiftout%(8)
DIM Ybase AS LONG, powersof2(11) AS LONG, WorkCode AS LONG
FOR a% = 0 TO 7: shiftout%(8 - a%) = 2 ^ a%: NEXT a%
FOR a% = 0 TO 11: powersof2(a%) = 2 ^ a%: NEXT a%
LET a$ = "images/kles2.gif"
IF a$ = "" THEN INPUT "GIF file"; a$: IF a$ = "" THEN END
IF INSTR(a$, ".") = 0 THEN a$ = a$ + ".gif"
OPEN a$ FOR BINARY AS #1
a$ = "      ": GET #1, , a$

GET #1, , TotalX: GET #1, , TotalY: GOSUB GetByte
NumColors = 2 ^ ((a% AND 7) + 1): NoPalette = (a% AND 128) = 0
GOSUB GetByte: Background = a%
GOSUB GetByte: IF a% <> 0 THEN PRINT "Bad  descriptor.": END
IF NoPalette = 0 THEN P$ = SPACE$(NumColors * 3): GET #1, , P$
DO
    GOSUB GetByte
    IF a% = 44 THEN
        EXIT DO
    ELSEIF a% <> 33 THEN
        PRINT "Unknown extension type.": END
    END IF
    GOSUB GetByte
    DO: GOSUB GetByte: a$ = SPACE$(a%): GET #1, , a$: LOOP UNTIL a% = 0
LOOP
GET #1, , XStart: GET #1, , YStart: GET #1, , XLength: GET #1, , YLength
XEnd = XStart + XLength: YEnd = YStart + YLength: GOSUB GetByte
IF a% AND 128 THEN PRINT "Can't handle local colormaps.": END
Interlaced = a% AND 64: PassNumber = 0: PassStep = 8
GOSUB GetByte
ClearCode = 2 ^ a%
EOSCode = ClearCode + 1
FirstCode = ClearCode + 2: NextCode = FirstCode
StartCodeSize = a% + 1: CodeSize = StartCodeSize
StartMaxCode = 2 ^ (a% + 1) - 1: MaxCode = StartMaxCode

BitsIn = 0: BlockSize = 0: BlockPointer = 1
x% = XStart: Y% = YStart: Ybase = Y% * 320&

SCREEN 13: DEF SEG = &HA000
IF NoPalette = 0 THEN
    OUT &H3C7, 0: OUT &H3C8, 0
    FOR a% = 1 TO NumColors * 3: OUT &H3C9, ASC(MID$(P$, a%, 1)) \ 4: NEXT a%
END IF
LINE (0, 0)-(319, 199), Background, BF
DO
    GOSUB GetCode
    IF code <> EOSCode THEN
        IF code = ClearCode THEN
            NextCode = FirstCode
            CodeSize = StartCodeSize
            MaxCode = StartMaxCode
            GOSUB GetCode
            CurCode = code: LastCode = code: LastPixel = code
            IF x% < 320 THEN POKE x% + Ybase, LastPixel
            x% = x% + 1: IF x% = XEnd THEN GOSUB NextScanLine
        ELSE
            CurCode = code: StackPointer = 0
            IF code > NextCode THEN EXIT DO
            IF code = NextCode THEN
                CurCode = LastCode
                OutStack(StackPointer) = LastPixel
                StackPointer = StackPointer + 1
            END IF

            DO WHILE CurCode >= FirstCode
                OutStack(StackPointer) = Suffix(CurCode)
                StackPointer = StackPointer + 1
                CurCode = Prefix(CurCode)
            LOOP

            LastPixel = CurCode
            IF x% < 320 THEN POKE x% + Ybase, LastPixel
            x% = x% + 1: IF x% = XEnd THEN GOSUB NextScanLine

            FOR a% = StackPointer - 1 TO 0 STEP -1
                IF x% < 320 THEN POKE x% + Ybase, OutStack(a%)
                x% = x% + 1: IF x% = XEnd THEN GOSUB NextScanLine
            NEXT a%

            IF NextCode < 4096 THEN
                Prefix(NextCode) = LastCode
                Suffix(NextCode) = LastPixel
                NextCode = NextCode + 1
                IF NextCode > MaxCode AND CodeSize < 12 THEN
                    CodeSize = CodeSize + 1
                    MaxCode = MaxCode * 2 + 1
                END IF
            END IF
            LastCode = code
        END IF
    END IF
LOOP UNTIL DoneFlag OR code = EOSCode

' Function and control key constants
GET (0, 0)-(200, 199), pictr
cd = 2
cs = 2
CONST ESC = 27, DOWN = 80, UP = 72, LEFT = 75, RIGHT = 77
CONST HOME = 71, ENDKEY = 79, PGDN = 81, PGUP = 73

DO
    ' Get a function or ASCII key
    DO
        Choice$ = INKEY$
    LOOP WHILE Choice$ = ""

    IF LEN(Choice$) = 1 THEN
        ' Handle ASCII keys
        SELECT CASE ASC(Choice$)
            CASE ESC
              GOSUB save
            CASE 65
              CLEAR : cd = 2: cs = 2
            CASE 97
              CLEAR : cd = 2: cs = 2
            CASE 13
              CIRCLE (a, B), 7, 4: PAINT (a, B), 4, 4: GET (0, 0)-(200, 199), pictr: COLOR 2: LINE (a - 5, B)-(a + 5, B): LINE (a, B - 5)-(a, B + 5)
            CASE 32
              CIRCLE (a, B), 7, 4: PAINT (a, B), 4, 4: GET (0, 0)-(200, 199), pictr: COLOR 2: LINE (a - 5, B)-(a + 5, B): LINE (a, B - 5)-(a, B + 5)
        END SELECT

    ELSE
        ' Convert 2-byte extended code to 1-byte ASCII code and handle
        Choice$ = RIGHT$(Choice$, 1)

        SELECT CASE Choice$
            CASE CHR$(DOWN)
                 PUT (0, 0), pictr, PSET: B = B + 5: COLOR 2: LINE (a - 5, B)-(a + 5, B): LINE (a, B - 5)-(a, B + 5): COLOR 15: LOCATE 1, 1: PRINT a; B
            CASE CHR$(UP)
                 PUT (0, 0), pictr, PSET: B = B - 5: COLOR 2: LINE (a - 5, B)-(a + 5, B): LINE (a, B - 5)-(a, B + 5): COLOR 15: LOCATE 1, 1: PRINT a; B
            CASE CHR$(RIGHT)
                 PUT (0, 0), pictr, PSET: a = a + 5: COLOR 2: LINE (a - 5, B)-(a + 5, B): LINE (a, B - 5)-(a, B + 5): COLOR 15: LOCATE 1, 1: PRINT a; B
            CASE CHR$(LEFT)
                 PUT (0, 0), pictr, PSET: a = a - 5: COLOR 2: LINE (a - 5, B)-(a + 5, B): LINE (a, B - 5)-(a, B + 5): COLOR 15: LOCATE 1, 1: PRINT a; B
            CASE ELSE
                BEEP
        END SELECT
    END IF
LOOP
















GetByte: a$ = " ": GET #1, , a$: a% = ASC(a$): RETURN
 
NextScanLine:
    IF Interlaced THEN
        Y% = Y% + PassStep
        IF Y% >= YEnd THEN
            PassNumber = PassNumber + 1
            SELECT CASE PassNumber
            CASE 1: Y% = 4: PassStep = 8
            CASE 2: Y% = 2: PassStep = 4
            CASE 3: Y% = 1: PassStep = 2
            END SELECT
        END IF
    ELSE
        Y% = Y% + 1
    END IF
    x% = XStart: Ybase = Y% * 320&: DoneFlag = Y% > 199
RETURN
GetCode:
    IF BitsIn = 0 THEN GOSUB ReadBufferedByte: LastChar = a%: BitsIn = 8
    WorkCode = LastChar \ shiftout%(BitsIn)
    DO WHILE CodeSize > BitsIn
        GOSUB ReadBufferedByte: LastChar = a%
        WorkCode = WorkCode OR LastChar * powersof2(BitsIn)
        BitsIn = BitsIn + 8
    LOOP
    BitsIn = BitsIn - CodeSize
    code = WorkCode AND MaxCode
RETURN
ReadBufferedByte:
    IF BlockPointer > BlockSize THEN
        GOSUB GetByte: BlockSize = a%
        a$ = SPACE$(BlockSize): GET #1, , a$
        BlockPointer = 1
    END IF
    a% = ASC(MID$(a$, BlockPointer, 1)): BlockPointer = BlockPointer + 1
RETURN

save:
DIM Cube(1 TO 100)
GET (114, 115)-(129, 125), Cube
DEF SEG = VARSEG(Cube(1))
BSAVE "AMERFLA4.PIC", VARPTR(Cube(1)), 25600
DEF SEG                 ' Restore default BASIC segment.
END

