'QB64 Interperater

'$include:'mem_library/mem_lib.bi'
'$include:'var_handler/var_hand.bi'

DIM SHARED alphanumeric(255)
FOR i = 48 TO 57
    alphanumeric(i) = -1
NEXT
FOR i = 65 TO 90
    alphanumeric(i) = -1
NEXT
FOR i = 97 TO 122
    alphanumeric(i) = -1
NEXT
'_ is treated as an alphabet letter
alphanumeric(95) = -1

DIM SHARED isalpha(255)
FOR i = 65 TO 90
    isalpha(i) = -1
NEXT
FOR i = 97 TO 122
    isalpha(i) = -1
NEXT
'_ is treated as an alphabet letter
isalpha(95) = -1

DIM SHARED isnumeric(255)
FOR i = 48 TO 57
    isnumeric(i) = -1
NEXT


DIM SHARED lfsinglechar(255)
lfsinglechar(40) = 1 '(
lfsinglechar(41) = 1 ')
lfsinglechar(42) = 1 '*
lfsinglechar(43) = 1 '+
lfsinglechar(45) = 1 '-
lfsinglechar(47) = 1 '/
lfsinglechar(60) = 1 '<
lfsinglechar(61) = 1 '=
lfsinglechar(62) = 1 '>
lfsinglechar(92) = 1 '\
lfsinglechar(94) = 1 '^

lfsinglechar(44) = 1 ',
lfsinglechar(46) = 1 '.
lfsinglechar(58) = 1 ':
lfsinglechar(59) = 1 ';

lfsinglechar(35) = 1 '# (file no only)
lfsinglechar(36) = 1 '$ (metacommand only)
lfsinglechar(63) = 1 '? (print macro)
lfsinglechar(95) = 1 '_

'--------------------------------- COMMANDS ------------------------------------
'$include:'commands\print.bi'
'$include:'commands\system.bi'






$CONSOLE

CONST DEBUG = -1
CONST VER$ = ".01"

'Globals
DIM SHARED QBI_BASIC_FILE$, QBI_STD_INPUT AS INTEGER, QBI_COMMAND_PARSE$
DIM SHARED QBI_Scope AS _UNSIGNED LONG


dbg_line "QB64 Interperater Ver:" + VER$

dbg_line "COMMAND$:" + COMMAND$
dbg_line "Parsing COMMAND$..."

cmd$ = COMMAND$
'Parse for "-the" flags and "--hthe-h" flags.
DO
  x = x + 1
  SELECT CASE mid$(cmd$, x, 1)
		case "-"
		  in_flag = -1
		case "f"
			if in_flag then
				do: x = x + 1: loop until mid$(cmd$, x, 1) <> " "
				if mid$(cmd$, x, 1) = chr$(34) then
					QBI_BASIC_FILE$ = mid$(cmd$, x, instr(mid$(cmd$, x + 1), chr$(34)) - 1)
					x = instr(mid$(cmd$, x + 1), chr$(34)) + 1
				elseif instr(mid$(cmd$, x), " ") > 0 then
					QBI_BASIC_FILE$ = mid$(cmd$, x, instr(mid$(cmd$, x + 1), " ") - 1)
					x = instr(mid$(cmd$, x + 1), " ")
				else
					QBI_BASIC_FILE$ = mid$(cmd$, x)
					x = len(cmd$)
				end if
				in_flag = 0
			else
				dbg_line "Parsed f without '-'..."
			end if
		case "c"
			if in_flag then
				do: x = x + 1: loop until mid$(cmd$, x, 1) <> " "
				if mid$(cmd$, x, 1) = chr$(34) then
					QBI_COMMAND_PARSE$ = mid$(cmd$, x, instr(mid$(cmd$, x + 1), chr$(34)) - 1)
					x = instr(mid$(cmd$, x + 1), chr$(34)) + 1
				elseif instr(mid$(cmd$, x), " ") > 0 then
					QBI_COMMAND_PARSE$ = mid$(cmd$, x, instr(mid$(cmd$, x + 1), " ") - 1)
					x = instr(mid$(cmd$, x + 1), " ")
				else
					QBI_COMMAND_PARSE$ = mid$(cmd$, x)
					x = len(cmd$)
				end if
				in_flag = 0
			else
				dbg_line "Parsed f without '-'..."
			end if
		case "i"
			if in_flag then
				'get from standard input
				QBI_STD_INPUT = -1
			else
				dbg_line "Parsed i without '-'..."
			end if
		case " "
			in_flag = 0
		case else
			if in_flag then
				dbg_line "Unknown flag " + mid$(cmd$, x, 1)
			end if
  END SELECT
LOOP until x > len(cmd$)
if QBI_COMMAND_PARSE$ > "" then
	dbg_line "COMMAND$ to use in interpreted program:" + QBI_COMMAND_PARSE$
end if

if QBI_STD_INPUT then
	dbg_line "Reading code from STD input"
	_SOURCE _CONSOLE
	std_input = -1
end if

if QBI_BASIC_FILE$ > "" then
	dbg_line "Reading code from file: " + QBI_BASIC_FILE$
	OPEN QBI_BASIC_FILE$ FOR input as #1
	from_file = -1
end if

end_flag = 0
_DEST 0
QBI_scope = 0
DO
	IF std_input then
		src& = _SOURCE 'backup _SOURCE and _DEST
		dst& = _DEST
		_SOURCE _CONSOLE
		_DEST _CONSOLE
		LINE INPUT a$ 'Grab our line from the console
		_SOURCE src& 'Restore _SOURCE and _DEST
		_DEST dst&
	elseif from_file then
		LINE INPUT #1, a$ 'Grab line from file
	end if

	l$ = lineformat$(a$)

	dbg_line "Parsed line: " + l$
	ul$ = ucase$(l$)
	token$ = mid$(ul$, 1, instr(ul$, " ") - 1)
	dbg_line "Token: " + token$



	select case token$
		case QBI_PRINT_TOKEN$
			dbg_line "Command Print"
			QBI_PRINT l$
		CASE QBI_SYSTEM_TOKEN$
			dbg_line "Command System"
			QBI_SYSTEM l$
		CASE ELSE 'possibly a variable
			if instr(mid$(l$, len(token$) + 1, 3), "=") then
				'variable
				'token$ is the variable's name
				dbg_line "Variable: " + token$
				ind = get_variable_index(token$, QBI_Scope)

			end if
	end select
	if std_input then
		if a$ = chr$(4) then 'eof character
			end_flag = -1
		end if
	else
		if EOF(1) then
			end_flag = -1
		end if
	end if
LOOP UNTIL end_flag


SYSTEM



SUB dbg_line (d$)
if debug then
	ds& = _DEST
	'_CONSOLE ON
	_DEST _CONSOLE
	PRINT d$
	'_CONSOLE OFF
	_DEST ds&
end if
END SUB


FUNCTION lineformat$ (a$)
dim sp as string, layoutcomment as string
sp = " "
a2$ = ""
linecontinuation = 0

continueline:

a$ = a$ + "  " 'add 2 extra spaces to make reading next char easier

ca$ = a$
a$ = UCASE$(a$)

n = LEN(a$)
i = 1
lineformatnext:
IF i >= n THEN GOTO lineformatdone

c = ASC(a$, i)
c$ = CHR$(c) '***remove later***

'----------------quoted string----------------
IF c = 34 THEN '"
    a2$ = a2$ + sp + CHR$(34)
    p1 = i + 1
    FOR i2 = i + 1 TO n - 2
        c2 = ASC(a$, i2)


        IF c2 = 34 THEN
            a2$ = a2$ + MID$(ca$, p1, i2 - p1 + 1) + "," + str2$(i2 - (i + 1))
            i = i2 + 1
            EXIT FOR
        END IF

        IF c2 = 92 THEN '\
            a2$ = a2$ + MID$(ca$, p1, i2 - p1) + "\\"
            p1 = i2 + 1
        END IF

        IF c2 < 32 OR c2 > 126 THEN
            o$ = OCT$(c2)
            IF LEN(o$) < 3 THEN
                o$ = "0" + o$
                IF LEN(o$) < 3 THEN o$ = "0" + o$
            END IF
            a2$ = a2$ + MID$(ca$, p1, i2 - p1) + "\" + o$
            p1 = i2 + 1
        END IF

    NEXT

    IF i2 = n - 1 THEN 'no closing "
        a2$ = a2$ + MID$(ca$, p1, (n - 2) - p1 + 1) + CHR$(34) + "," + str2$((n - 2) - (i + 1) + 1)
        i = n - 1
    END IF

    GOTO lineformatnext

END IF

'----------------number----------------
firsti = i
IF c = 46 THEN
    c2$ = MID$(a$, i + 1, 1): c2 = ASC(c2$)
    IF (c2 >= 48 AND c2 <= 57) THEN GOTO lfnumber
END IF
IF (c >= 48 AND c <= 57) THEN '0-9
    lfnumber:

    'handle 'IF a=1 THEN a=2 ELSE 100' by assuming numeric after ELSE to be a
    IF RIGHT$(a2$, 5) = sp + "ELSE" THEN
        a2$ = a2$ + sp + "GOTO"
    END IF

    'Number will be converted to the following format:
    ' 999999  .        99999  E        +         999
    '[whole$][dp(0/1)][frac$][ed(1/2)][pm(1/-1)][ex$]
    ' 0                1               2         3    <-mode

    mode = 0
    whole$ = ""
    dp = 0
    frac$ = ""
    ed = 0 'E=1, D=2, F=3
    pm = 1
    ex$ = ""




    lfreadnumber:
    valid = 0

    IF c = 46 THEN
        IF mode = 0 THEN valid = 1: dp = 1: mode = 1
    END IF

    IF c >= 48 AND c <= 57 THEN '0-9
        valid = 1
        IF mode = 0 THEN whole$ = whole$ + c$
        IF mode = 1 THEN frac$ = frac$ + c$
        IF mode = 2 THEN mode = 3
        IF mode = 3 THEN ex$ = ex$ + c$
    END IF

    IF c = 69 OR c = 68 OR c = 70 THEN 'E,D,F
        IF mode < 2 THEN
            valid = 1
            IF c = 69 THEN ed = 1
            IF c = 68 THEN ed = 2
            IF c = 70 THEN ed = 3
            mode = 2
        END IF
    END IF

    IF c = 43 OR c = 45 THEN '+,-
        IF mode = 2 THEN
            valid = 1
            IF c = 45 THEN pm = -1
            mode = 3
        END IF
    END IF

    IF valid THEN
        IF i <= n THEN i = i + 1: c$ = MID$(a$, i, 1): c = ASC(c$): GOTO lfreadnumber
    END IF



    'cull leading 0s off whole$
    DO WHILE LEFT$(whole$, 1) = "0": whole$ = RIGHT$(whole$, LEN(whole$) - 1): LOOP
    'cull trailing 0s off frac$
    DO WHILE RIGHT$(frac$, 1) = "0": frac$ = LEFT$(frac$, LEN(frac$) - 1): LOOP
    'cull leading 0s off ex$
    DO WHILE LEFT$(ex$, 1) = "0": ex$ = RIGHT$(ex$, LEN(ex$) - 1): LOOP

    IF dp <> 0 OR ed <> 0 THEN float = 1 ELSE float = 0

    extused = 1

    IF ed THEN e$ = "": GOTO lffoundext 'no extensions valid after E/D/F specified

    '3-character extensions
    IF i <= n - 2 THEN
        e$ = MID$(a$, i, 3)
        IF e$ = "~%%" AND float = 0 THEN i = i + 3: GOTO lffoundext
        IF e$ = "~&&" AND float = 0 THEN i = i + 3: GOTO lffoundext
        IF e$ = "~%&" AND float = 0 THEN Give_Error "Cannot use _OFFSET symbols after numbers": EXIT FUNCTION
    END IF
    '2-character extensions
    IF i <= n - 1 THEN
        e$ = MID$(a$, i, 2)
        IF e$ = "%%" AND float = 0 THEN i = i + 2: GOTO lffoundext
        IF e$ = "~%" AND float = 0 THEN i = i + 2: GOTO lffoundext
        IF e$ = "&&" AND float = 0 THEN i = i + 2: GOTO lffoundext
        IF e$ = "~&" AND float = 0 THEN i = i + 2: GOTO lffoundext
        IF e$ = "%&" AND float = 0 THEN Give_Error "Cannot use _OFFSET symbols after numbers": EXIT FUNCTION
        IF e$ = "##" THEN
            i = i + 2
            ed = 3
            e$ = ""
            GOTO lffoundext
        END IF
        IF e$ = "~`" THEN
            i = i + 2
            GOTO lffoundbitext
        END IF
    END IF
    '1-character extensions
    IF i <= n THEN
        e$ = MID$(a$, i, 1)
        IF e$ = "%" AND float = 0 THEN i = i + 1: GOTO lffoundext
        IF e$ = "&" AND float = 0 THEN i = i + 1: GOTO lffoundext
        IF e$ = "!" THEN
            i = i + 1
            ed = 1
            e$ = ""
            GOTO lffoundext
        END IF
        IF e$ = "#" THEN
            i = i + 1
            ed = 2
            e$ = ""
            GOTO lffoundext
        END IF
        IF e$ = "`" THEN
            i = i + 1
            lffoundbitext:
            bitn$ = ""
            DO WHILE i <= n
                c2 = ASC(MID$(a$, i, 1))
                IF c2 >= 48 AND c2 <= 57 THEN
                    bitn$ = bitn$ + CHR$(c2)
                    i = i + 1
                ELSE
                    EXIT DO
                END IF
            LOOP
            IF bitn$ = "" THEN bitn$ = "1"
            'cull leading 0s off bitn$
            DO WHILE LEFT$(bitn$, 1) = "0": bitn$ = RIGHT$(bitn$, LEN(bitn$) - 1): LOOP
            e$ = e$ + bitn$
            GOTO lffoundext
        END IF
    END IF

    IF float THEN 'floating point types CAN be assumed
        'calculate first significant digit offset & number of significant digits
        IF whole$ <> "" THEN
            offset = LEN(whole$) - 1
            sigdigits = LEN(whole$) + LEN(frac$)
        ELSE
            IF frac$ <> "" THEN
                offset = -1
                sigdigits = LEN(frac$)
                FOR i2 = 1 TO LEN(frac$)
                    IF MID$(frac$, i2, 1) <> "0" THEN EXIT FOR
                    offset = offset - 1
                    sigdigits = sigdigits - 1
                NEXT
            ELSE
                'number is 0
                offset = 0
                sigdigits = 0
            END IF
        END IF
        sigdig$ = RIGHT$(whole$ + frac$, sigdigits)
        'SINGLE?
        IF sigdigits <= 7 THEN 'QBASIC interprets anything with more than 7 sig. digits as a DOUBLE
            IF offset <= 38 AND offset >= -38 THEN 'anything outside this range cannot be represented as a SINGLE
                IF offset = 38 THEN
                    IF sigdig$ > "3402823" THEN GOTO lfxsingle
                END IF
                IF offset = -38 THEN
                    IF sigdig$ < "1175494" THEN GOTO lfxsingle
                END IF
                ed = 1
                e$ = ""
                GOTO lffoundext
            END IF
        END IF
        lfxsingle:
        'DOUBLE?
        IF sigdigits <= 16 THEN 'QB64 handles DOUBLES with 16-digit precision
            IF offset <= 308 AND offset >= -308 THEN 'anything outside this range cannot be represented as a DOUBLE
                IF offset = 308 THEN
                    IF sigdig$ > "1797693134862315" THEN GOTO lfxdouble
                END IF
                IF offset = -308 THEN
                    IF sigdig$ < "2225073858507201" THEN GOTO lfxdouble
                END IF
                ed = 2
                e$ = ""
                GOTO lffoundext
            END IF
        END IF
        lfxdouble:
        'assume _FLOAT
        ed = 3
        e$ = "": GOTO lffoundext
    END IF

    extused = 0
    e$ = ""
    lffoundext:

    'make sure a leading numberic character exists
    IF whole$ = "" THEN whole$ = "0"
    'if a float, ensure frac$<>"" and dp=1
    IF float THEN
        dp = 1
        IF frac$ = "" THEN frac$ = "0"
    END IF
    'if ed is specified, make sure ex$ exists
    IF ed <> 0 AND ex$ = "" THEN ex$ = "0"

    a2$ = a2$ + sp
    a2$ = a2$ + whole$
    IF dp THEN a2$ = a2$ + "." + frac$
    IF ed THEN
        IF ed = 1 THEN a2$ = a2$ + "E"
        IF ed = 2 THEN a2$ = a2$ + "D"
        IF ed = 3 THEN a2$ = a2$ + "F"
        IF pm = -1 AND ex$ <> "0" THEN a2$ = a2$ + "-" ELSE a2$ = a2$ + "+"
        a2$ = a2$ + ex$
    END IF
    a2$ = a2$ + e$

    IF extused THEN a2$ = a2$ + "," + MID$(a$, firsti, i - firsti)

    GOTO lineformatnext
END IF

'----------------(number)&H...----------------
'note: the final value, not the number of hex characters, sets the default type
IF c = 38 THEN '&
    IF MID$(a$, i + 1, 1) = "H" THEN
        i = i + 2
        hx$ = ""
        lfreadhex:
        IF i <= n THEN
            c$ = MID$(a$, i, 1): c = ASC(c$)
            IF (c >= 48 AND c <= 57) OR (c >= 65 AND c <= 70) THEN hx$ = hx$ + c$: i = i + 1: GOTO lfreadhex
        END IF
        fullhx$ = "&H" + hx$

        'cull leading 0s off hx$
        DO WHILE LEFT$(hx$, 1) = "0": hx$ = RIGHT$(hx$, LEN(hx$) - 1): LOOP
        IF hx$ = "" THEN hx$ = "0"

        bitn$ = ""
        '3-character extensions
        IF i <= n - 2 THEN
            e$ = MID$(a$, i, 3)
            IF e$ = "~%%" THEN i = i + 3: GOTO lfhxext
            IF e$ = "~&&" THEN i = i + 3: GOTO lfhxext
            IF e$ = "~%&" THEN Give_Error "Cannot use _OFFSET symbols after numbers": EXIT FUNCTION
        END IF
        '2-character extensions
        IF i <= n - 1 THEN
            e$ = MID$(a$, i, 2)
            IF e$ = "%%" THEN i = i + 2: GOTO lfhxext
            IF e$ = "~%" THEN i = i + 2: GOTO lfhxext
            IF e$ = "&&" THEN i = i + 2: GOTO lfhxext
            IF e$ = "%&" THEN Give_Error "Cannot use _OFFSET symbols after numbers": EXIT FUNCTION
            IF e$ = "~&" THEN i = i + 2: GOTO lfhxext
            IF e$ = "~`" THEN
                i = i + 2
                GOTO lfhxbitext
            END IF
        END IF
        '1-character extensions
        IF i <= n THEN
            e$ = MID$(a$, i, 1)
            IF e$ = "%" THEN i = i + 1: GOTO lfhxext
            IF e$ = "&" THEN i = i + 1: GOTO lfhxext
            IF e$ = "`" THEN
                i = i + 1
                lfhxbitext:
                DO WHILE i <= n
                    c2 = ASC(MID$(a$, i, 1))
                    IF c2 >= 48 AND c2 <= 57 THEN
                        bitn$ = bitn$ + CHR$(c2)
                        i = i + 1
                    ELSE
                        EXIT DO
                    END IF
                LOOP
                IF bitn$ = "" THEN bitn$ = "1"
                'cull leading 0s off bitn$
                DO WHILE LEFT$(bitn$, 1) = "0": bitn$ = RIGHT$(bitn$, LEN(bitn$) - 1): LOOP
                GOTO lfhxext
            END IF
        END IF
        'if no valid extension context was given, assume one
        'note: leading 0s have been culled, so LEN(hx$) reflects its values size
        e$ = "&&"
        IF LEN(hx$) <= 8 THEN e$ = "&" 'as in QBASIC, signed values must be used
        IF LEN(hx$) <= 4 THEN e$ = "%" 'as in QBASIC, signed values must be used
        GOTO lfhxext2
        lfhxext:
        fullhx$ = fullhx$ + e$ + bitn$
        lfhxext2:

        'build 8-byte unsigned integer rep. of hx$
        IF LEN(hx$) > 16 THEN Give_Error "Overflow": EXIT FUNCTION
        v~&& = 0
        FOR i2 = 1 TO LEN(hx$)
            v2 = ASC(MID$(hx$, i2, 1))
            IF v2 <= 57 THEN v2 = v2 - 48 ELSE v2 = v2 - 65 + 10
            v~&& = v~&& * 16 + v2
        NEXT

        finishhexoctbin:
        num$ = str2u64$(v~&&) 'correct for unsigned values (overflow of unsigned can be checked later)
        IF LEFT$(e$, 1) <> "~" THEN 'note: range checking will be performed later in fixop.order
            'signed

            IF e$ = "%%" THEN
                IF v~&& > 127 THEN
                    IF v~&& > 255 THEN Give_Error "Overflow": EXIT FUNCTION
                    v~&& = ((NOT v~&&) AND 255) + 1
                    num$ = "-" + sp + str2u64$(v~&&)
                END IF
            END IF

            IF e$ = "%" THEN
                IF v~&& > 32767 THEN
                    IF v~&& > 65535 THEN Give_Error "Overflow": EXIT FUNCTION
                    v~&& = ((NOT v~&&) AND 65535) + 1
                    num$ = "-" + sp + str2u64$(v~&&)
                END IF
            END IF

            IF e$ = "&" THEN
                IF v~&& > 2147483647 THEN
                    IF v~&& > 4294967295 THEN Give_Error "Overflow": EXIT FUNCTION
                    v~&& = ((NOT v~&&) AND 4294967295) + 1
                    num$ = "-" + sp + str2u64$(v~&&)
                END IF
            END IF

            IF e$ = "&&" THEN
                IF v~&& > 9223372036854775807 THEN
                    'note: no error checking necessary
                    v~&& = (NOT v~&&) + 1
                    num$ = "-" + sp + str2u64$(v~&&)
                END IF
            END IF

            IF e$ = "`" THEN
                vbitn = VAL(bitn$)
                h~&& = 1: FOR i2 = 1 TO vbitn - 1: h~&& = h~&& * 2: NEXT: h~&& = h~&& - 1 'build h~&&
                IF v~&& > h~&& THEN
                    h~&& = 1: FOR i2 = 1 TO vbitn: h~&& = h~&& * 2: NEXT: h~&& = h~&& - 1 'build h~&&
                    IF v~&& > h~&& THEN Give_Error "Overflow": EXIT FUNCTION
                    v~&& = ((NOT v~&&) AND h~&&) + 1
                    num$ = "-" + sp + str2u64$(v~&&)
                END IF
            END IF

        END IF '<>"~"

        a2$ = a2$ + sp + num$ + e$ + bitn$ + "," + fullhx$

        GOTO lineformatnext
    END IF
END IF

'----------------(number)&O...----------------
'note: the final value, not the number of oct characters, sets the default type
IF c = 38 THEN '&
    IF MID$(a$, i + 1, 1) = "O" THEN
        i = i + 2
        'note: to avoid mistakes, hx$ is used instead of 'ot$'
        hx$ = ""
        lfreadoct:
        IF i <= n THEN
            c$ = MID$(a$, i, 1): c = ASC(c$)
            IF c >= 48 AND c <= 55 THEN hx$ = hx$ + c$: i = i + 1: GOTO lfreadoct
        END IF
        fullhx$ = "&O" + hx$

        'cull leading 0s off hx$
        DO WHILE LEFT$(hx$, 1) = "0": hx$ = RIGHT$(hx$, LEN(hx$) - 1): LOOP
        IF hx$ = "" THEN hx$ = "0"

        bitn$ = ""
        '3-character extensions
        IF i <= n - 2 THEN
            e$ = MID$(a$, i, 3)
            IF e$ = "~%%" THEN i = i + 3: GOTO lfotext
            IF e$ = "~&&" THEN i = i + 3: GOTO lfotext
            IF e$ = "~%&" THEN Give_Error "Cannot use _OFFSET symbols after numbers": EXIT FUNCTION
        END IF
        '2-character extensions
        IF i <= n - 1 THEN
            e$ = MID$(a$, i, 2)
            IF e$ = "%%" THEN i = i + 2: GOTO lfotext
            IF e$ = "~%" THEN i = i + 2: GOTO lfotext
            IF e$ = "&&" THEN i = i + 2: GOTO lfotext
            IF e$ = "%&" THEN Give_Error "Cannot use _OFFSET symbols after numbers": EXIT FUNCTION
            IF e$ = "~&" THEN i = i + 2: GOTO lfotext
            IF e$ = "~`" THEN
                i = i + 2
                GOTO lfotbitext
            END IF
        END IF
        '1-character extensions
        IF i <= n THEN
            e$ = MID$(a$, i, 1)
            IF e$ = "%" THEN i = i + 1: GOTO lfotext
            IF e$ = "&" THEN i = i + 1: GOTO lfotext
            IF e$ = "`" THEN
                i = i + 1
                lfotbitext:
                bitn$ = ""
                DO WHILE i <= n
                    c2 = ASC(MID$(a$, i, 1))
                    IF c2 >= 48 AND c2 <= 57 THEN
                        bitn$ = bitn$ + CHR$(c2)
                        i = i + 1
                    ELSE
                        EXIT DO
                    END IF
                LOOP
                IF bitn$ = "" THEN bitn$ = "1"
                'cull leading 0s off bitn$
                DO WHILE LEFT$(bitn$, 1) = "0": bitn$ = RIGHT$(bitn$, LEN(bitn$) - 1): LOOP
                GOTO lfotext
            END IF
        END IF
        'if no valid extension context was given, assume one
        'note: leading 0s have been culled, so LEN(hx$) reflects its values size
        e$ = "&&"
        '37777777777
        IF LEN(hx$) <= 11 THEN
            IF LEN(hx$) < 11 OR ASC(LEFT$(hx$, 1)) <= 51 THEN e$ = "&"
        END IF
        '177777
        IF LEN(hx$) <= 6 THEN
            IF LEN(hx$) < 6 OR LEFT$(hx$, 1) = "1" THEN e$ = "%"
        END IF

        GOTO lfotext2
        lfotext:
        fullhx$ = fullhx$ + e$ + bitn$
        lfotext2:

        'build 8-byte unsigned integer rep. of hx$
        '1777777777777777777777 (22 digits)
        IF LEN(hx$) > 22 THEN Give_Error "Overflow": EXIT FUNCTION
        IF LEN(hx$) = 22 THEN
            IF LEFT$(hx$, 1) <> "1" THEN Give_Error "Overflow": EXIT FUNCTION
        END IF
        '********change v& to v~&&********
        v~&& = 0
        FOR i2 = 1 TO LEN(hx$)
            v2 = ASC(MID$(hx$, i2, 1))
            v2 = v2 - 48
            v~&& = v~&& * 8 + v2
        NEXT

        GOTO finishhexoctbin
    END IF
END IF

'----------------(number)&B...----------------
'note: the final value, not the number of bin characters, sets the default type
IF c = 38 THEN '&
    IF MID$(a$, i + 1, 1) = "B" THEN
        i = i + 2
        'note: to avoid mistakes, hx$ is used instead of 'bi$'
        hx$ = ""
        lfreadbin:
        IF i <= n THEN
            c$ = MID$(a$, i, 1): c = ASC(c$)
            IF c >= 48 AND c <= 49 THEN hx$ = hx$ + c$: i = i + 1: GOTO lfreadbin
        END IF
        fullhx$ = "&B" + hx$

        'cull leading 0s off hx$
        DO WHILE LEFT$(hx$, 1) = "0": hx$ = RIGHT$(hx$, LEN(hx$) - 1): LOOP
        IF hx$ = "" THEN hx$ = "0"

        bitn$ = ""
        '3-character extensions
        IF i <= n - 2 THEN
            e$ = MID$(a$, i, 3)
            IF e$ = "~%%" THEN i = i + 3: GOTO lfbiext
            IF e$ = "~&&" THEN i = i + 3: GOTO lfbiext
            IF e$ = "~%&" THEN Give_Error "Cannot use _OFFSET symbols after numbers": EXIT FUNCTION
        END IF
        '2-character extensions
        IF i <= n - 1 THEN
            e$ = MID$(a$, i, 2)
            IF e$ = "%%" THEN i = i + 2: GOTO lfbiext
            IF e$ = "~%" THEN i = i + 2: GOTO lfbiext
            IF e$ = "&&" THEN i = i + 2: GOTO lfbiext
            IF e$ = "%&" THEN Give_Error "Cannot use _OFFSET symbols after numbers": EXIT FUNCTION
            IF e$ = "~&" THEN i = i + 2: GOTO lfbiext
            IF e$ = "~`" THEN
                i = i + 2
                GOTO lfbibitext
            END IF
        END IF


        '1-character extensions
        IF i <= n THEN
            e$ = MID$(a$, i, 1)
            IF e$ = "%" THEN i = i + 1: GOTO lfbiext
            IF e$ = "&" THEN i = i + 1: GOTO lfbiext
            IF e$ = "`" THEN
                i = i + 1
                lfbibitext:
                bitn$ = ""
                DO WHILE i <= n
                    c2 = ASC(MID$(a$, i, 1))
                    IF c2 >= 48 AND c2 <= 57 THEN
                        bitn$ = bitn$ + CHR$(c2)
                        i = i + 1
                    ELSE
                        EXIT DO
                    END IF
                LOOP
                IF bitn$ = "" THEN bitn$ = "1"
                'cull leading 0s off bitn$
                DO WHILE LEFT$(bitn$, 1) = "0": bitn$ = RIGHT$(bitn$, LEN(bitn$) - 1): LOOP
                GOTO lfbiext
            END IF
        END IF
        'if no valid extension context was given, assume one
        'note: leading 0s have been culled, so LEN(hx$) reflects its values size
        e$ = "&&"
        IF LEN(hx$) <= 32 THEN e$ = "&"
        IF LEN(hx$) <= 16 THEN e$ = "%"

        GOTO lfbiext2
        lfbiext:
        fullhx$ = fullhx$ + e$ + bitn$
        lfbiext2:

        'build 8-byte unsigned integer rep. of hx$
        IF LEN(hx$) > 64 THEN Give_Error "Overflow": EXIT FUNCTION

        v~&& = 0
        FOR i2 = 1 TO LEN(hx$)
            v2 = ASC(MID$(hx$, i2, 1))
            v2 = v2 - 48
            v~&& = v~&& * 2 + v2
        NEXT

        GOTO finishhexoctbin
    END IF
END IF


'----------------(number)&H??? error----------------
IF c = 38 THEN Give_Error "Expected &H... or &O...": EXIT FUNCTION

'----------------variable/name----------------
'*trailing _ is treated as a seperate line extension*
IF (c >= 65 AND c <= 90) OR c = 95 THEN 'A-Z(a-z) or _
    IF c = 95 THEN p2 = 0 ELSE p2 = i
    FOR i2 = i + 1 TO n
        c2 = ASC(a$, i2)
        IF NOT alphanumeric(c2) THEN EXIT FOR
        IF c2 <> 95 THEN p2 = i2
    NEXT
    IF p2 THEN 'not just underscores!
        'char is from i to p2
        n2 = p2 - i + 1
        a3$ = MID$(a$, i, n2)

        '----(variable/name)rem----
        IF n2 = 3 THEN
            IF a3$ = "REM" THEN
                i = i + n2
                'note: In QBASIC 'IF cond THEN REM comment' counts as a single line IF statement, however use of ' instead of REM does not
                IF UCASE$(RIGHT$(a2$, 5)) = sp + "THEN" THEN a2$ = a2$ + sp + "'" 'add nop
                layoutcomment = "REM"
                GOTO comment
            END IF
        END IF

        '----(variable/name)data----
        IF n2 = 4 THEN
            IF a3$ = "DATA" THEN
                x$ = ""
                i = i + n2
                scan = 0
                speechmarks = 0
                commanext = 0
                finaldata = 0
                e$ = ""
                p1 = 0
                p2 = 0
                nextdatachr:
                IF i < n THEN
                    c = ASC(a$, i)

                    IF c = 9 OR c = 32 THEN
                        IF scan = 0 THEN GOTO skipwhitespace
                    END IF

                    IF c = 58 THEN '":"
                        IF speechmarks = 0 THEN finaldata = 1: GOTO adddata
                    END IF

                    IF c = 44 THEN '","
                        IF speechmarks = 0 THEN
                            adddata:
                            IF prepass = 0 THEN
                                IF p1 THEN
                                    'FOR i2 = p1 TO p2
                                    '    DATA_add ASC(ca$, i2)
                                    'NEXT
                                    x$ = x$ + MID$(ca$, p1, p2 - p1 + 1)
                                END IF
                                'assume closing "
                                IF speechmarks THEN
                                    'DATA_add 34
                                    x$ = x$ + CHR$(34)
                                END IF
                                'append comma
                                'DATA_add 44
                                x$ = x$ + CHR$(44)
                            END IF
                            IF finaldata = 1 THEN GOTO finisheddata
                            e$ = ""
                            p1 = 0
                            p2 = 0
                            speechmarks = 0
                            scan = 0
                            commanext = 0
                            i = i + 1
                            GOTO nextdatachr
                        END IF
                    END IF '","

                    IF commanext = 1 THEN
                        IF c <> 32 AND c <> 9 THEN Give_Error "Expected , after quoted string in DATA statement": EXIT FUNCTION
                    END IF

                    IF c = 34 THEN
                        IF speechmarks = 1 THEN
                            commanext = 1
                            speechmarks = 0
                        END IF
                        IF scan = 0 THEN speechmarks = 1
                    END IF

                    scan = 1

                    IF p1 = 0 THEN p1 = i: p2 = i
                    IF c <> 9 AND c <> 32 THEN p2 = i

                    skipwhitespace:
                    i = i + 1: GOTO nextdatachr
                END IF 'i<n
                finaldata = 1: GOTO adddata
                finisheddata:
                e$ = ""
                IF prepass = 0 THEN
                    PUT #16, , x$
                    DataOffset = DataOffset + LEN(x$)

                    e$ = SPACE$((LEN(x$) - 1) * 2)
                    FOR ec = 1 TO LEN(x$) - 1
                        '2 chr hex encode each character
                        v1 = ASC(x$, ec)
                        v2 = v1 \ 16: IF v2 <= 9 THEN v2 = v2 + 48 ELSE v2 = v2 + 55
                        v1 = v1 AND 15: IF v1 <= 9 THEN v1 = v1 + 48 ELSE v1 = v1 + 55
                        ASC(e$, ec * 2 - 1) = v1
                        ASC(e$, ec * 2) = v2
                    NEXT

                END IF

                a2$ = a2$ + sp + "DATA": IF LEN(e$) THEN a2$ = a2$ + sp + "_" + e$
                GOTO lineformatnext
            END IF
        END IF

        a2$ = a2$ + sp + MID$(ca$, i, n2)
        i = i + n2

        '----(variable/name)extensions----
        extcheck:
        IF n2 > 40 THEN Give_Error "Identifier longer than 40 character limit": EXIT FUNCTION
        c3 = ASC(a$, i)
        m = 0
        IF c3 = 126 THEN '"~"
            e2$ = MID$(a$, i + 1, 2)
            IF e2$ = "&&" THEN e2$ = "~&&": GOTO lfgetve
            IF e2$ = "%%" THEN e2$ = "~%%": GOTO lfgetve
            IF e2$ = "%&" THEN e2$ = "~%&": GOTO lfgetve
            e2$ = CHR$(ASC(e2$))
            IF e2$ = "&" THEN e2$ = "~&": GOTO lfgetve
            IF e2$ = "%" THEN e2$ = "~%": GOTO lfgetve
            IF e2$ = "`" THEN m = 1: e2$ = "~`": GOTO lfgetve
        END IF
        IF c3 = 37 THEN
            c4 = ASC(a$, i + 1)
            IF c4 = 37 THEN e2$ = "%%": GOTO lfgetve
            IF c4 = 38 THEN e2$ = "%&": GOTO lfgetve
            e2$ = "%": GOTO lfgetve
        END IF
        IF c3 = 38 THEN
            c4 = ASC(a$, i + 1)
            IF c4 = 38 THEN e2$ = "&&": GOTO lfgetve
            e2$ = "&": GOTO lfgetve
        END IF
        IF c3 = 33 THEN e2$ = "!": GOTO lfgetve
        IF c3 = 35 THEN
            c4 = ASC(a$, i + 1)
            IF c4 = 35 THEN e2$ = "##": GOTO lfgetve
            e2$ = "#": GOTO lfgetve
        END IF
        IF c3 = 36 THEN m = 1: e2$ = "$": GOTO lfgetve
        IF c3 = 96 THEN m = 1: e2$ = "`": GOTO lfgetve
        '(no symbol)

        'cater for unusual names/labels (eg a.0b%)
        IF ASC(a$, i) = 46 THEN '"."
            c2 = ASC(a$, i + 1)
            IF c2 >= 48 AND c2 <= 57 THEN
                'scan until no further alphanumerics
                p2 = i + 1
                FOR i2 = i + 2 TO n
                    c = ASC(a$, i2)

                    IF NOT alphanumeric(c) THEN EXIT FOR
                    IF c <> 95 THEN p2 = i2 'don't including trailing _
                NEXT
                a2$ = a2$ + sp + "." + sp + MID$(ca$, i + 1, p2 - (i + 1) + 1) 'case sensitive
                n2 = n2 + 1 + (p2 - (i + 1) + 1)
                i = p2 + 1
                GOTO extcheck 'it may have an extension or be continued with another "."
            END IF
        END IF

        GOTO lineformatnext

        lfgetve:
        i = i + LEN(e2$)
        a2$ = a2$ + e2$
        IF m THEN 'allow digits after symbol
            lfgetvd:
            IF i < n THEN
                c = ASC(a$, i)
                IF c >= 48 AND c <= 57 THEN a2$ = a2$ + CHR$(c): i = i + 1: GOTO lfgetvd
            END IF
        END IF 'm

        GOTO lineformatnext

    END IF 'p2
END IF 'variable/name
'----------------variable/name end----------------

'----------------spacing----------------
IF c = 32 OR c = 9 THEN i = i + 1: GOTO lineformatnext

'----------------symbols----------------
'--------single characters--------
IF lfsinglechar(c) THEN

    IF c = 60 THEN '<
        c2 = ASC(a$, i + 1)
        IF c2 = 61 THEN a2$ = a2$ + sp + "<=": i = i + 2: GOTO lineformatnext
        IF c2 = 62 THEN a2$ = a2$ + sp + "<>": i = i + 2: GOTO lineformatnext
    END IF
    IF c = 62 THEN '>
        c2 = ASC(a$, i + 1)
        IF c2 = 61 THEN a2$ = a2$ + sp + ">=": i = i + 2: GOTO lineformatnext
        IF c2 = 60 THEN a2$ = a2$ + sp + "<>": i = i + 2: GOTO lineformatnext '>< to <>
    END IF
    IF c = 61 THEN '=
        c2 = ASC(a$, i + 1)
        IF c2 = 62 THEN a2$ = a2$ + sp + ">=": i = i + 2: GOTO lineformatnext '=> to >=
        IF c2 = 60 THEN a2$ = a2$ + sp + "<=": i = i + 2: GOTO lineformatnext '=< to <=
    END IF

    IF c = 36 AND LEN(a2$) THEN GOTO badusage '$

    a2$ = a2$ + sp + CHR$(c)
    i = i + 1
    GOTO lineformatnext
END IF
badusage:

IF c <> 39 THEN Give_Error "Unexpected character on line": EXIT FUNCTION 'invalid symbol encountered

'----------------comment(')----------------
layoutcomment = "'"
i = i + 1
comment:
IF i >= n THEN GOTO lineformatdone2
c$ = RIGHT$(a$, LEN(a$) - i + 1)
cc$ = RIGHT$(ca$, LEN(ca$) - i + 1)
IF LEN(c$) = 0 THEN GOTO lineformatdone2
layoutcomment$ = RTRIM$(layoutcomment$ + cc$)

c$ = LTRIM$(c$)
IF LEN(c$) = 0 THEN GOTO lineformatdone2
ac = ASC(c$)
IF ac <> 36 THEN GOTO lineformatdone2
nocasec$ = LTRIM$(RIGHT$(ca$, LEN(ca$) - i + 1))
memmode = 0
FOR x = 1 TO LEN(c$)
    mcnext:
    IF MID$(c$, x, 1) = "$" THEN

        'note: $STATICksdcdweh$DYNAMIC is valid!

        IF MID$(c$, x, 7) = "$STATIC" THEN
            memmode = 1
            xx = INSTR(x + 1, c$, "$")
            if xx=0 then exit for else
            x = xx: GOTO mcnext
        END IF

        IF MID$(c$, x, 8) = "$DYNAMIC" THEN
            memmode = 2
            xx = INSTR(x + 1, c$, "$")
            IF xx = 0 THEN EXIT FOR
            x = xx: GOTO mcnext
        END IF

        IF MID$(c$, x, 8) = "$INCLUDE" THEN
            'note: INCLUDE adds the file AFTER the line it is on has been processed
            'note: No other metacommands can follow the INCLUDE metacommand!
            'skip spaces until :
            FOR xx = x + 8 TO LEN(c$)
                ac = ASC(MID$(c$, xx, 1))
                IF ac = 58 THEN EXIT FOR ':
                IF ac <> 32 AND ac <> 9 THEN Give_Error "Expected $INCLUDE:'filename'": EXIT FUNCTION
            NEXT
            x = xx
            'skip spaces until '
            FOR xx = x + 1 TO LEN(c$)
                ac = ASC(MID$(c$, xx, 1))
                IF ac = 39 THEN EXIT FOR 'character:'
                IF ac <> 32 AND ac <> 9 THEN Give_Error "Expected $INCLUDE:'filename'": EXIT FUNCTION
            NEXT
            x = xx
            xx = INSTR(x + 1, c$, "'")
            IF xx = 0 THEN Give_Error "Expected $INCLUDE:'filename'": EXIT FUNCTION
            addmetainclude$ = MID$(nocasec$, x + 1, xx - x - 1)
            IF addmetainclude$ = "" THEN Give_Error "Expected $INCLUDE:'filename'": EXIT FUNCTION
            GOTO mcfinal
        END IF

        'add more metacommands here

    END IF '$
NEXT
mcfinal:

IF memmode = 1 THEN addmetastatic = 1
IF memmode = 2 THEN addmetadynamic = 1

GOTO lineformatdone2



lineformatdone:

'line continuation?
'note: line continuation in idemode is illegal

'Line continuation not supported by interpreter currently
'IF LEN(a2$) THEN
'    IF RIGHT$(a2$, 1) = "_" THEN

'        linecontinuation = 1 'avoids auto-format glitches
'        layout$ = ""

 '       'remove _ from the end of the building string
 '       IF LEN(a2$) >= 2 THEN
 '           IF RIGHT$(a2$, 2) = sp + "_" THEN a2$ = LEFT$(a2$, LEN(a2$) - 1)
 '       END IF
 '       a2$ = LEFT$(a2$, LEN(a2$) - 1)

'        IF inclevel THEN
'            fh = 99 + inclevel
'            IF EOF(fh) THEN GOTO lineformatdone2
'            LINE INPUT #fh, a$
'            inclinenumber(inclevel) = inclinenumber(inclevel) + 1
'            GOTO includecont 'note: should not increase linenumber
'        END IF

'        IF idemode THEN
'            idecommand$ = CHR$(100)
'            ignore = ide(0)
'            ideerror = 0
'            a$ = idereturn$
'            IF a$ = "" THEN GOTO lineformatdone2
'        ELSE
'            a$ = lineinput3$
'            IF a$ = CHR$(13) THEN GOTO lineformatdone2
'        END IF

'        linenumber = linenumber + 1

        includecont:

'        contline = 1
'        GOTO continueline
'    END IF
'END IF

lineformatdone2:
IF LEFT$(a2$, 1) = sp THEN a2$ = RIGHT$(a2$, LEN(a2$) - 1)

'fix for trailing : error
IF RIGHT$(a2$, 1) = ":" THEN a2$ = a2$ + sp + "'" 'add nop

'IF Debug THEN PRINT #9, "lineformat():return:" + a2$
IF Error_Happened THEN EXIT FUNCTION
lineformat$ = a2$

END FUNCTION

SUB give_error (e$)
dbg_line "ERROR: " + e$

END SUB

FUNCTION str2$ (v AS LONG)
str2$ = LTRIM$(RTRIM$(STR$(v)))
END FUNCTION

FUNCTION str2u64$ (v~&&)
str2u64$ = LTRIM$(RTRIM$(STR$(v~&&)))
END FUNCTION

FUNCTION str2i64$ (v&&)
str2i64$ = LTRIM$(RTRIM$(STR$(v&&)))
END FUNCTION

FUNCTION grab_str$(s$, length)
grab_str$ = mid$(s$, 2, instr(mid$(s$, 2), chr$(34)) - 1)
length = val(mid$(s$, len(grab_str$) + 3))
END FUNCTION

'MEMORY LIBRARY
'$include:'mem_library/mem_lib.bm'

'VARIABLE HANDLER
'$include:'var_handler/var_hand.bm'

'--------------------------------- COMMANDS ------------------------------------
'$include:'commands\print.bm'
'$include:'commands\system.bm'
