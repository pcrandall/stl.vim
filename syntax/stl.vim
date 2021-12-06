if exists("b:current_syntax")
  finish
endif

syn keyword stlDataType BOOL BYTE WORD DWORD INT DINT REAL S5TIME TIME DATE CHAR TIMER ANY POINTER VOID ARRAY OF
syn keyword stlNetwork NETWORK
syn keyword stlBegin BEGIN
syn keyword stlConfig  AUTHOR FAMILY NAME VERSION
syn keyword stlBlockType  FUNCTION FUNCTION_BLOCK DATA_BLOCK
syn keyword stlBlockType  END_FUNCTION END_FUNCTION_BLOCK END_DATA_BLOCK
syn keyword stlIOType  VAR_INPUT VAR_OUTPUT VAR_IN_OUT VAR_STAT VAR_TEMP END_VAR
syn keyword stlInstructions ABS ACOS AD AN ASIN ATAN AUF AW BE BEA BEB BEC BEU BLD BTD BTI CAD CALL CAR CAW CC CD CDB CLR COS CU DEC DTB DTR ENT EXP FN FP FR INC INVD INVI ITB ITD JBI JC JCB JCN JL JM JMZ JN JNB JNBI JO JOS JP JPZ JU JUO JZ LAR1 LAR1AR LAR1AR2 LAR2 LC LDBLG LDBNO LDILG LDINO LEAVE LN LOOP LSTW MCRA MCRD MOD NEGD NEGI NEGR NOP0 NOP1 NOT OD ON OPN OW POP PUSH RLD RLDA RND RRD RRDA SA SAVE SD SE SET SF SI SIN SLD SLW SP SPA SPB SPBB SPBI SPBIN SPBN SPBNB SPL SPM SPMZ SPN SPO SPP SPPZ SPS SPU SPZ SQR SQRT SRD SRW SS SSD SSI SV TAD TAK TAN TAR TAR1 TAR2 TAW TDB TRUNC TSTW UC UD UN UW XN XOD XOW ZR ZV

syn match stlOp  "[\[\]()=+-/]\w*"
syn match stlOp "[<>=+*-/][DIR]"
syn match stlOp "[<>=][=][DIR]"
syn match stlOp "\s[ALORSTUX]\s"
syn match stlOp "\s[ALORSTUX]("
syn match stlComment "//.*$"
syn match stlWord "[a-fA-F]+"
syn match stlNumber "0x[0-9a-fA-F]\+"
syn match stlNumber "[WL]#[0-9a-fA-F]\+"
syn match stlNumber "\<[0-9]\+D\=\>"
syn match stlNumber "L#[0-9]\+"
syn match stlNumber "W#[0-9a-fA-F]\+#[0-9a-fA-F]\+"
syn match stlNumber "\d\+\.\d*"
syn match stlLabelName "^[a-zA-Z0-9_]\+:" contained
syn match stlTitle 'TITLE.*$'
syn match stlPointer "P#[#a-zA-Z0-9\.]*"
syn match stlDeclaration "^\s*[a-zA-Z0-9_]*\:"
syn match stlAssignment "\s*[a-zA-Z0-9_ ]*\s\:"

syn match stlArr "\[.*\]"

syn region stlString start='"' end='"'
syn region stlLabel start="^[a-zA-Z0-9_]" end=":" oneline contains=stlLabelName,stlInstructions,stlNumber,stlComment,stlOp,stlAssignment,stlArr,stlPointer


syntax cluster stlExpressions contains=@stlAssignment,@stlTypeItems,@stlVarItems

let b:current_syntax = "stl"
hi def link stlComment      Comment
hi def link stlLabelName    Todo
hi def link stlDataType     Type
hi def link stlBlockType    Constant
hi def link stlNetwork      Error
hi def link stlConfig       PreProc
hi def link stlIOType       Constant
hi def link stlNumber       Number
hi def link stlString       String
hi def link stlTitle        Underlined
hi def link stlInstructions Keyword
hi def link stlOp           Keyword
hi def link stlDeclaration  Identifier
hi def link stlArr          Number
hi def link stlPointer      Constant
hi def link stlAssignment   Identifier
hi def link stlArguments    String
