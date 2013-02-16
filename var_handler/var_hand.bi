'VARIABLES
TYPE var_type
	'vt is equal to one of the const values below
  vt as INTEGER
  s as _UNSIGNED INTEGER 'scope -- value indicating the value value -- 0 is 'MAIN', higher means nested scopes like FUNCTION's/SUB's
  Global as _BYTE 'if set, then variable was declare global and should take precedence
  'Global is also used for STATIC values
  n AS MEM_string_type 'variables name
  m as _MEM 'memory for this variable
END TYPE

CONST UNSIGN = 1        '10000000
CONST BIT_TYPE = 2       '01000000
CONST BYTE_TYPE = 4      '00100000
CONST INTEGER_TYPE = 6   '01100000
CONST LONG_TYPE = 8      '00010000
CONST INT64_TYPE = 10    '01010000
CONST SINGLE_TYPE = 12   '00110000
CONST DOUBLE_TYPE = 14   '01110000
CONST FLOAT_TYPE = 16    '00001000
CONST STRING_TYPE = 18   '01001000
CONST STRING_N_TYPE = 19 '10001000

REDIM SHARED QBI_VARIABLES(100) as var_type, QBI_Variable_count AS _UNSIGNED LONG
