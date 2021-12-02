if exists("b:current_syntax")
  finish
endif

syn keyword stlDataType BOOL BYTE WORD DWORD INT DINT REAL S5TIME TIME DATE CHAR TIMER ANY POINTER VOID
syn keyword stlNetwork NETWORK
syn keyword stlConfig  AUTHOR FAMILY NAME VERSION BEGIN
syn keyword stlBlockType  FUNCTION FUNCTION_BLOCK DATA_BLOCK
syn keyword stlBlockType  END_FUNCTION END_FUNCTION_BLOCK END_DATA_BLOCK
syn keyword stlInstructions A ABS ACOS AD AN ASIN ATAN AUF AW BE BEA BEB BEC BEU BLD BTD BTI CAD CALL CAR CAW CC CD CDB CLR COS CU DEC DTB DTR ENT EXP FN FP FR INC INVD INVI ITB ITD JBI JC JCB JCN JL JM JMZ JN JNB JNBI JO JOS JP JPZ JU JUO JZ L LAR1 LAR1AR LAR1AR2 LAR2 LC LDBLG LDBNO LDILG LDINO LEAVE LN LOOP LSTW MCRA MCRD MOD NEGD NEGI NEGR NOP0 NOP1 NOT O OD ON OPN OW POP PUSH R RLD RLDA RND RRD RRDA S SA SAVE SD SE SET SF SI SIN SLD SLW SP SPA SPB SPBB SPBI SPBIN SPBN SPBNB SPL SPM SPMZ SPN SPO SPP SPPZ SPS SPU SPZ SQR SQRT SRD SRW SS SSD SSI SV T TAD TAK TAN TAR TAR1 TAR2 TAW TDB TRUNC TSTW U UC UD UN UW X XN XOD XOW ZR ZV

syn match stlOp  "[()=+-/]\w*"
syn match stlOp "[<>=+*-/][DIR]"
syn match stlOp "[<>=][=][DIR]"
syn match stlComment "//.*$"
syn match stlWord "[a-fA-F]+"
syn match stlNumber "0x[0-9a-fA-F]\+"
syn match stlNumber "\<[0-9]\+D\=\>"
syn match stlLabelName "^[a-zA-Z0-9_]\+:" contained
syn match stlAddrLabel ':[a-zA-Z0-9\._]\*\;'
syn match stlTitle 'TITLE.*$'

syn region stlLabel start="^[a-zA-Z0-9_]" end=":" oneline contains=stlLabelName,stlComment,stlInstructions,stlNumber,stlNetwork,stlOp
syn region stlString start='"' end='"'

let b:current_syntax = "stl"
hi def link stlComment      Comment
hi def link stlLabelName    Todo
hi def link stlAddrLabel    Error
hi def link stlLabel        Identifier
hi def link stlDataType     Type
hi def link stlBlockType    Statement
hi def link stlNetwork      Error
hi def link stlConfig       PreProc
hi def link stlNumber       Number
hi def link stlString       String
hi def link stlInstructions Statement
hi def link stlOp           Statement
hi def link stlTitle        Underlined
