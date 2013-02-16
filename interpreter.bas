'QB64 Interperater

$CONSOLE

CONST DEBUG = -1
CONST VER$ = ".01"

DIM SHARED QBI_BASIC_FILE$, QBI_STD_INPUT AS INTEGER

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
		case else
			if in_flag then
				dbg_line "Unknown flag " + mid$(cmd$, x, 1)
			end if
  END SELECT
LOOP until x > len(cmd$)
if QBI_STD_INPUT then
	dbg_line "Reading code from STD input"
elseif QBI_BASIC_FILE$ > "" then
	dbg_line "Reading code from file: " + QBI_BASIC_FILE$
end if

SYSTEM



SUB dbg_line (d$)
if debug then
	ds& = _DEST
	_DEST _CONSOLE
	PRINT d$
	_DEST ds&
end if
END SUB
