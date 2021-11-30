if exists("b:current_syntax")
  finish
endif

" STL syntax is case-insenstitive
syntax case ignore

"  {{{ POU declaration
syn keyword STLPOUKeyword FUNCTION FUNCTION_BLOCK DATA_BLOCK
syn keyword STLPOUKeyword END_FUNCTION END_FUNCTION_BLOCK END_DATA_BLOCK

syn keyword STLSpecial BEGIN NETWORK TITLE AUTHOR FAMILY NAME VERSION
" }}}

" {{{ Data types identifiers
" Elementary data types
syn keyword STLTypeInteger SINT INT DINT LINE USINT UINT UDINT ULINT
syn keyword STLTypeReal REAL LREAL
syn keyword STLTypeDate TIME DATE TIME_OF_DAY TOD DATE_AND_TIME DT
syn keyword STLTypeDate LTIME LDATE LTIME_OF_DAY LTOD LDATE_AND_TIME LDT
syn keyword STLTypeString BOOL BYTE WORD DWORD LWORD
syn keyword STLTypeString STRING WSTRING CHAR WCHAR
" Generic data types
syn keyword STLTypeGeneric ANY ANY_DERIVED ANY_ELEMENTARY ANY_MAGNITUDE
syn keyword STLTypeGeneric ANY_NUM ANY_REAL ANY_INT ANY_BIT ANY_STRING ANY_DATE
" Derived (user-specified) data types
syn keyword STLTypeDerived TYPE STRUCT
syn keyword STLTypeDerived END_TYPE END_STRUCT
" }}}

" {{{ Data types literals
syn keyword STLBoolean TRUE FALSE
" }}}


" {{{ Variable declaration
syn keyword STLVarKeyword VAR VAR_INPUT VAR_OUTPUT VAR_IN_OUT VAR_TEMP VAR_STAT
syn keyword STLVarKeyword END_VAR
" }}}

" {{{ Duration literals
syn region  STLDuration start="#\(\-\)\=[0-9]\{1,2}\(\-[0-9]\{1,2}\)\{-\}[mshd(ms)]" end="[ ,]"he=e-1 contains=STLTypeDate
" }}}

" {{{ Expressions clusters
syntax cluster STLExpressions contains=@STLPOUItems,@STLTypeItems,@STLVarItems
syntax cluster STLPOUItems contains=STLPOUKeyword
syntax cluster STLTypeItems contains=STLTypeInteger,STLTypeReal,STLTypeDate,STLTypeString,STLTypeGeneric,STLTypeDerived
syntax cluster STLVarItems contains=STLVarKeyword
" }}}


" {{{ 'Common element' regions
syntax region STLElementType start="\<TYPE\>" end="\<END_TYPE\>" contains=@STLExpressions fold
syntax region STLElementVar start="\<VAR\>" end="\<END_VAR\>" contains=@STLExpressions fold
syntax region STLElementVarIn start="\<VAR_INPUT\>" end="\<END_VAR\>" contains=@STLExpressions fold
syntax region STLElementVarOut start="\<VAR_OUTPUT\>" end="\<END_VAR\>" contains=@STLExpressions fold
syntax region STLElementVarInOut start="\<VAR_IN_OUT\>" end="\<END_VAR\>" contains=@STLExpressions fold
syntax region STLElementVarExternal start="\<VAR_EXTERNAL\>" end="\<END_VAR\>" contains=@STLExpressions fold
syntax region STLElementVarTemp start="\<VAR_TEMP\>" end="\<END_VAR\>" contains=@STLExpressions fold
syntax region STLElementVarAccess start="\<VAR_ACCESS\>" end="\<END_VAR\>" contains=@STLExpressions fold
syntax region STLElementVarGlobal start="\<VAR_GLOBAL\>" end="\<END_VAR\>" contains=@STLExpressions fold
syntax region STLElementFunction start="\<FUNCTION\>" end="\<END_FUNCTION\>" contains=@STLExpressions fold
syntax region STLElementFunctionBlock start="\<FUNCTION_BLOCK\>" end="\<END_FUNCTION_BLOCK\>" contains=@STLExpressions fold
syntax region STLElementProgram start="\<PROGRAM\>" end="\<END_PROGRAM\>" contains=@STLExpressions fold
syntax region STLElementStep start="\<STEP\>" end="\<END_STEP\>" contains=@STLExpressions fold
syntax region STLElementTransition start="\<TRANSITION\>" end="\<END_TRANSITION\>" contains=@STLExpressions fold
syntax region STLElementAction start="\<Action\>" end="\<END_ACTION\>" contains=@STLExpressions fold
" }}}


" {{{ STL Operators
syn keyword STLOperator LD LDN ST STN S R AND ANDN OR ORN XOR XORN NOT ADD SUB
syn keyword STLOperator MUL DIV MOD GT GE EQ NE LE LT JMP JMPC JMPCN CAL CALC
syn keyword STLOperator CALCN RET RETC RETCN

syn keyword STOperator NOT MOD AND XOR OR
" ST statements
syn keyword STConditional IF ELSIF ELSE END_IF CASE END_CASE THEN TO
syn keyword STRepeat FOR WHILE REPEAT END_FOR END_WHILE END_REPEAT BY DO DO UNTIL
syn keyword STStatement EXIT CONTINUE RETURN
syn keyword STStatement OVERLAP REF_TO NULL
" }}}

" {{{ Comments
syn region STLComment start="(\*" end="\*)"
" }}}

" {{{ Highlighting
hi def link STLPOUKeyword           Function
hi def link STLSpecial              Special
hi def link STLAccessSpecifier      Identifier
hi def link STLVarKeyword           Keyword
hi def link STLConf                 Special
hi def link STLConfTask             Function
hi def link STLConfTaskOpt          Keyword
hi def link STLConfTargetName       Identifier
" Data types identifiers
hi def link STLTypeInteger          Type
hi def link STLTypeReal             Type
hi def link STLTypeDate             Type
hi def link STLTypeString           Type
hi def link STLTypeGeneric          Struct
hi def link STLTypeDerived          Type
" Data types literals
hi def link STLDuration             String
hi def link STLBoolean              Boolean
" ST
hi def link STOperator              Statement
hi def link STConditional           Conditional
hi def link STRepeat                Repeat
hi def link STStatement             Statement
" SFC
hi def link SFCElement              Statement
" IL
hi def link STLOperator              Statement
" Comments
hi def link STLComment              Comment
" }}}

let b:current_syntax = "stl"

" vim: foldmethod=marker sw=2

" JDH-8 ASSEMBLER SYNTAX FILE

if exists("b:current_syntax")
  finish
endif

syn keyword jdhSpecial \$
syn keyword jdhReg a b c d f h l z
syn keyword jdhTodo contained TODO
syn match jdhComment ";.*$" contains=jdhTodo
syn match jdhDirective "^\s*[@][a-zA-Z]\+"
syn match jdhMacroArg "%[a-zA-Z0-9_]\+"
syn match jdhNumber "0x[0-9a-fA-F]\+"
syn match jdhNumber "\<[0-9]\+D\=\>"
syn match jdhOp "^\s*[a-zA-Z0-9_]\+\s"
syn match jdhOp "^\s*[a-zA-Z0-9_]\+$"
syn match jdhMicroOp "^\s*[~]\=[a-zA-Z0-9_]\+[,]\s*\([~]\=[a-zA-Z0-9_]\+[,]\=\s*\)\+"

" syn region jdhOp start='^' end='$'
syn region jdhLabel start="^\s*[a-zA-Z0-9_.]" end=":" oneline contains=jdhLabelName,jdhMacroArg,jdhAddr
syn region jdhString start='"' end='"'
syn region jdhAddr start='\[' end='\]' contains=jdhMacroArg,jdhAddrLabel

syn match jdhLabelName "^[a-zA-Z0-9_\.]\+:\=" contained
syn match jdhAddrLabel '\<[a-zA-Z0-9\._]\+\>' contained

let b:current_syntax = "jdh8"
hi def link jdhSpecial Special
hi def link jdhTodo Todo
hi def link jdhComment Comment
hi def link jdhLabelName Label
hi def link jdhAddrLabel Label
hi def link jdhDirective Macro
hi def link jdhOp Keyword
hi def link jdhMicroOp Keyword
hi def link jdhMacroArg Special
hi def link jdhReg Keyword
hi def link jdhNumber Number
hi def link jdhString String
