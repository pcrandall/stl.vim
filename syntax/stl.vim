if exists("b:current_syntax")
  finish
endif

syn keyword stlDataType BOOL BYTE WORD DWORD INT DINT REAL S5TIME TIME DATE CHAR TIMER ANY POINTER VOID
syn keyword stlNetwork NETWORK
syn keyword stlConfig  AUTHOR FAMILY NAME VERSION BEGIN
syn keyword stlInstructions ) )MCR *D *I *R + +AR1 +AR2 +D +I +R /D /I /R <=D <=I <=R <>D <>I <>R <D <I <R = ==D ==I ==R >=D >=I >=R >D >I >R A A( ABS ACOS AD AN AN( ASIN ATAN AUF AW BE BEA BEB BEC BEU BLD BTD BTI CAD CALL CAR CAW CC CD CDB CLR COS CU DEC DTB DTR ENT EXP FN FP FR INC INVD INVI ITB ITD JBI JC JCB JCN JL JM JMZ JN JNB JNBI JO JOS JP JPZ JU JUO JZ L LAR1 LAR1<D> LAR1AR LAR1AR2 LAR2 LAR2<D> LC LDBLG LDBNO LDILG LDINO LEAVE LN LOOP LSTW MCR( MCRA MCRD MOD NEGD NEGI NEGR NOP0 NOP1 NOT O O( OD ON ON( OPN OW POP PUSH R RLD RLDA RND RND+ RND– RRD RRDA S SA SAVE SD SE SET SF SI SIN SLD SLW SP SPA SPB SPBB SPBI SPBIN SPBN SPBNB SPL SPM SPMZ SPN SPO SPP SPPZ SPS SPU SPZ SQR SQRT SRD SRW SS SSD SSI SV T TAD TAK TAN TAR TAR1 TAR2 TAW TDB TRUNC TSTW U U( UC UD UN UN( UW X X( XN XN( XOD XOW ZR ZV –D –I –R

" syn keyword stlReg a b c d f h l z
syn match stlComment "//.*$"
syn match stlNumber "0x[0-9a-fA-F]\+"
syn match stlNumber "\<[0-9]\+D\=\>"
syn match stlLabelName "^[a-zA-Z0-9_]\+:" contained
syn match stlAddrLabel ':[a-zA-Z0-9\._]\*\;'
syn match stlTitle 'TITLE.*$'
" syn match stlDirective "^\s*[@][a-zA-Z]\+"
" syn match stlMacroArg "%[a-zA-Z0-9_]\+"
" syn match stlOp "^\s*[a-zA-Z0-9_]\+\s"
" syn match stlOp "^\s*[a-zA-Z0-9_]\+$"
" syn match stlMicroOp "^\s*[~]\=[a-zA-Z0-9_]\+[,]\s*\([~]\=[a-zA-Z0-9_]\+[,]\=\s*\)\+"

" syn region stlOp start='^' end='$'
syn region stlLabel start="^[a-zA-Z0-9_]" end=":" oneline contains=stlLabelName,stlComment,stlInstructions,stlNumber,stlNetwork
syn region stlAddr start='\[' end='\]' contains=stlNumber
syn region stlString start='"' end='"'



let b:current_syntax = "stl"
hi def link stlComment      Comment
hi def link stlLabelName    Todo
hi def link stlAddrLabel    Error
hi def link stlLabel        Identifier
hi def link stlDataType     Type
hi def link stlNetwork      Error
hi def link stlConfig       PreProc
hi def link stlNumber       Number
hi def link stlString       String
hi def link stlInstructions Statement
hi def link stlExpressions  Underlined
hi def link stlTitle        String

" hi def link stlLabel        Underlined
" hi def link stlDirective    Macro
" hi def link stlOp           Keyword
" hi def link stlMicroOp      Keyword
" hi def link stlMacroArg     Special
" hi def link stlReg          Keyword
"hi def link stlTodo Todo


" if exists("b:current_syntax")
"   finish
" endif

" " STL syntax is case-insenstitive
" syntax case ignore

" "  {{{ POU declaration
" syn keyword STLPOUKeyword FUNCTION FUNCTION_BLOCK DATA_BLOCK
" syn keyword STLPOUKeyword END_FUNCTION END_FUNCTION_BLOCK END_DATA_BLOCK

" syn keyword STLSpecial BEGIN NETWORK TITLE AUTHOR FAMILY NAME VERSION
" " }}}

" " {{{ Data types identifiers
" " Elementary data types
" syn keyword STLTypeInteger SINT INT DINT LINE USINT UINT UDINT ULINT
" syn keyword STLTypeReal REAL LREAL
" syn keyword STLTypeDate TIME DATE TIME_OF_DAY TOD DATE_AND_TIME DT
" syn keyword STLTypeDate LTIME LDATE LTIME_OF_DAY LTOD LDATE_AND_TIME LDT
" syn keyword STLTypeString BOOL BYTE WORD DWORD LWORD
" syn keyword STLTypeString STRING WSTRING CHAR WCHAR
" " Generic data types
" syn keyword STLTypeGeneric ANY ANY_DERIVED ANY_ELEMENTARY ANY_MAGNITUDE
" syn keyword STLTypeGeneric ANY_NUM ANY_REAL ANY_INT ANY_BIT ANY_STRING ANY_DATE
" " Derived (user-specified) data types
" syn keyword STLTypeDerived TYPE STRUCT
" syn keyword STLTypeDerived END_TYPE END_STRUCT
" " }}}

" " {{{ Data types literals
" syn keyword STLBoolean TRUE FALSE
" " }}}


" " {{{ Variable declaration
" syn keyword STLVarKeyword VAR VAR_INPUT VAR_OUTPUT VAR_IN_OUT VAR_TEMP VAR_STAT
" syn keyword STLVarKeyword END_VAR
" " }}}

" " {{{ Duration literals
" syn region  STLDuration start="#\(\-\)\=[0-9]\{1,2}\(\-[0-9]\{1,2}\)\{-\}[mshd(ms)]" end="[ ,]"he=e-1 contains=STLTypeDate
" " }}}

" " {{{ Expressions clusters
" syntax cluster STLExpressions contains=@STLPOUItems,@STLTypeItems,@STLVarItems
" syntax cluster STLPOUItems contains=STLPOUKeyword
" syntax cluster STLTypeItems contains=STLTypeInteger,STLTypeReal,STLTypeDate,STLTypeString,STLTypeGeneric,STLTypeDerived
" syntax cluster STLVarItems contains=STLVarKeyword
" " }}}


" " {{{ 'Common element' regions
" syntax region STLElementType start="\<TYPE\>" end="\<END_TYPE\>" contains=@STLExpressions fold
" syntax region STLElementVar start="\<VAR\>" end="\<END_VAR\>" contains=@STLExpressions fold
" syntax region STLElementVarIn start="\<VAR_INPUT\>" end="\<END_VAR\>" contains=@STLExpressions fold
" syntax region STLElementVarOut start="\<VAR_OUTPUT\>" end="\<END_VAR\>" contains=@STLExpressions fold
" syntax region STLElementVarInOut start="\<VAR_IN_OUT\>" end="\<END_VAR\>" contains=@STLExpressions fold
" syntax region STLElementVarExternal start="\<VAR_EXTERNAL\>" end="\<END_VAR\>" contains=@STLExpressions fold
" syntax region STLElementVarTemp start="\<VAR_TEMP\>" end="\<END_VAR\>" contains=@STLExpressions fold
" syntax region STLElementVarAccess start="\<VAR_ACCESS\>" end="\<END_VAR\>" contains=@STLExpressions fold
" syntax region STLElementVarGlobal start="\<VAR_GLOBAL\>" end="\<END_VAR\>" contains=@STLExpressions fold
" syntax region STLElementFunction start="\<FUNCTION\>" end="\<END_FUNCTION\>" contains=@STLExpressions fold
" syntax region STLElementFunctionBlock start="\<FUNCTION_BLOCK\>" end="\<END_FUNCTION_BLOCK\>" contains=@STLExpressions fold
" syntax region STLElementProgram start="\<PROGRAM\>" end="\<END_PROGRAM\>" contains=@STLExpressions fold
" syntax region STLElementStep start="\<STEP\>" end="\<END_STEP\>" contains=@STLExpressions fold
" syntax region STLElementTransition start="\<TRANSITION\>" end="\<END_TRANSITION\>" contains=@STLExpressions fold
" syntax region STLElementAction start="\<Action\>" end="\<END_ACTION\>" contains=@STLExpressions fold
" " }}}


" " {{{ STL Operators
" syn keyword STLOperator LD LDN ST STN S R AND ANDN OR ORN XOR XORN NOT ADD SUB
" syn keyword STLOperator MUL DIV MOD GT GE EQ NE LE LT JMP JMPC JMPCN CAL CALC
" syn keyword STLOperator CALCN RET RETC RETCN

" syn keyword STOperator NOT MOD AND XOR OR
" " ST statements
" syn keyword STConditional IF ELSIF ELSE END_IF CASE END_CASE THEN TO
" syn keyword STRepeat FOR WHILE REPEAT END_FOR END_WHILE END_REPEAT BY DO DO UNTIL
" syn keyword STStatement EXIT CONTINUE RETURN
" syn keyword STStatement OVERLAP REF_TO NULL
" " }}}

" " {{{ Comments
" syn region STLComment start="(\*" end="\*)"
" " }}}

" " {{{ Highlighting
" hi def link STLPOUKeyword           Function
" hi def link STLSpecial              Special
" hi def link STLAccessSpecifier      Identifier
" hi def link STLVarKeyword           Keyword
" hi def link STLConf                 Special
" hi def link STLConfTask             Function
" hi def link STLConfTaskOpt          Keyword
" hi def link STLConfTargetName       Identifier
" " Data types identifiers
" hi def link STLTypeInteger          Type
" hi def link STLTypeReal             Type
" hi def link STLTypeDate             Type
" hi def link STLTypeString           Type
" hi def link STLTypeGeneric          Struct
" hi def link STLTypeDerived          Type
" " Data types literals
" hi def link STLDuration             String
" hi def link STLBoolean              Boolean
" " ST
" hi def link STOperator              Statement
" hi def link STConditional           Conditional
" hi def link STRepeat                Repeat
" hi def link STStatement             Statement
" " SFC
" hi def link SFCElement              Statement
" " IL
" hi def link STLOperator              Statement
" " Comments
" hi def link STLComment              Comment
" " }}}
"
"
"
" "  {{{ POU declaration
" syn keyword stlPOUKeyword FUNCTION FUNCTION_BLOCK DATA_BLOCK
" syn keyword stlPOUKeyword END_FUNCTION END_FUNCTION_BLOCK END_DATA_BLOCK

" syn keyword stlAccessSpecifier PUBLIC PRIVATE PROTECTED INTERNAL
" syn keyword stlSpecial OVERRIDE ABSTRACT FINAL EXTENDS IMPLEMENTS
" " }}}

" " {{{ Data types identifiers
" " Elementary data types
" syn keyword stlTypeInteger SINT INT DINT LINE USINT UINT UDINT ULINT
" syn keyword stlTypeReal REAL LREAL
" syn keyword stlTypeDate TIME DATE TIME_OF_DAY TOD DATE_AND_TIME DT
" syn keyword stlTypeDate LTIME LDATE LTIME_OF_DAY LTOD LDATE_AND_TIME LDT
" syn keyword stlTypeString BOOL BYTE WORD DWORD LWORD
" syn keyword stlTypeString STRING WSTRING CHAR WCHAR
" " Generic data types
" syn keyword stlTypeGeneric ANY ANY_DERIVED ANY_ELEMENTARY ANY_MAGNITUDE
" syn keyword stlTypeGeneric ANY_NUM ANY_REAL ANY_INT ANY_BIT ANY_STRING ANY_DATE
" " Derived (user-specified) data types
" syn keyword stlTypeDerived TYPE STRUCT
" syn keyword stlTypeDerived END_TYPE END_STRUCT
" syn keyword stlTypeDerived ARRAY OF
" " }}}
" syntax cluster stlExpressions contains=@stlPOUItems,@stlTypeItems,@stlVarItems
" syntax cluster stlPOUItems contains=stlPOUKeyword
" syntax cluster stlTypeItems contains=stlTypeInteger,stlTypeReal,stlTypeDate,stlTypeString,stlTypeGeneric,stlTypeDerived
" syntax cluster stlVarItems contains=stlVarKeyword

" syntax region STLElementType start="\<TYPE\>" end="\<END_TYPE\>" contains=@STLExpressions fold
" syntax region STLElementVar start="\<VAR\>" end="\<END_VAR\>" contains=@STLExpressions fold
" syntax region STLElementVarIn start="\<VAR_INPUT\>" end="\<END_VAR\>" contains=@STLExpressions fold
" syntax region STLElementVarOut start="\<VAR_OUTPUT\>" end="\<END_VAR\>" contains=@STLExpressions fold
" syntax region STLElementVarInOut start="\<VAR_IN_OUT\>" end="\<END_VAR\>" contains=@STLExpressions fold
" syntax region STLElementVarExternal start="\<VAR_EXTERNAL\>" end="\<END_VAR\>" contains=@STLExpressions fold
" syntax region STLElementVarTemp start="\<VAR_TEMP\>" end="\<END_VAR\>" contains=@STLExpressions fold
" syntax region STLElementVarAccess start="\<VAR_ACCESS\>" end="\<END_VAR\>" contains=@STLExpressions fold
" syntax region STLElementVarGlobal start="\<VAR_GLOBAL\>" end="\<END_VAR\>" contains=@STLExpressions fold
" syntax region STLElementFunction start="\<FUNCTION\>" end="\<END_FUNCTION\>" contains=@STLExpressions fold
" syntax region STLElementFunctionBlock start="\<FUNCTION_BLOCK\>" end="\<END_FUNCTION_BLOCK\>" contains=@STLExpressions fold
" syntax region STLElementProgram start="\<DATA_BLOCK\>" end="\<END_DATA_BLOCK\>" contains=@STLExpressions fold

