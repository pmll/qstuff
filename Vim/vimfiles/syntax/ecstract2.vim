" Vim syntax file for Ecstract2
" --- made from ecstract3 version so probably mostly wrong
" --- also probably mostly wrong because the help is mostly wrong

" Quit when a syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

" identifiers
syn match ecVarConst		'[a-z][a-zA-Z0-9]*'
syn match ecVarConst		'[A-Z][A-Z0-9_]*[A-Z]'
syn match ecProcFunc		'[A-Z][a-zA-Z0-9]*'

" I think syntax folding will have to go in favour of expr folding, this is
" good enough for now as it allows the major blocks to be folded away
" level 1 folding
"syn region ecLevel1Fold	matchgroup=Statement fold transparent start="\v^(proc|func|init|body|tail|table)" end="^end" contains=ALL

" Compiler directives
syn keyword ecComp	codeFile include 
syn region ecComp	start="?define " end="$"
syn region ecComp	start="?undefine " end="$"
syn region ecComp	start="?if " end="$"
syn region ecComp	start="?else" end="$"
syn region ecComp	start="?end" end="$"

" ecstract statements
syn keyword ecStatement	abort append archive attach
syn keyword ecStatement	baseFont box break
syn keyword ecStatement	case chain chainAll clear clearScreen com
syn keyword ecStatement	const create critical
syn keyword ecStatement dataPanel dbFind delete
syn keyword ecStatement	deleteAssociated display
" up to here in checking keywords
syn keyword ecStatement dynamic document dms
syn keyword ecStatement editGrid email ensure enter excelLink exclusive
syn keyword ecStatement exclusiveAssociated exit export extend external
syn keyword ecStatement	f8 f8Suffixed fail fetch find flush font footer for
syn keyword ecStatement form format friendlyName
syn keyword ecStatement getValue getXY grandTotal graph grid group
syn keyword ecStatement	header hyperlink
syn keyword ecStatement	if image import index input invoke
syn keyword ecStatement	library line link list listFolder load loadAssociated
syn keyword ecStatement	loop
syn keyword ecStatement	mailTo message moneyBox
syn keyword ecStatement namePair navigation newLine new noExposing noteFormat
syn keyword ecStatement null
syn keyword ecStatement	open order outlook output override
syn keyword ecStatement	packedValue page paragraph pause pickGrid point
syn keyword ecStatement progress prompt
syn keyword ecStatement redisplay register remember renameFile report resolve
syn keyword ecStatement	retrieve ruleLine
syn keyword ecStatement	save saveAssociated screen section separator setValue	
syn keyword ecStatement	setXY sheet showTables singleton sleep sms soap
syn keyword ecStatement	soapAction soapExecute soapNamespace status stopwatch
syn keyword ecStatement	subtotal switch
syn keyword ecStatement to transfer type
syn keyword ecStatement	update
syn keyword ecStatement	validate value var view viewGrid
syn keyword ecStatement	wake watch web webExecute webParameter while with
syn keyword ecStatement	wordLink write
syn keyword ecStatement where by also
syn keyword ecStatement end then else suppress of at stubborn one static table
syn keyword ecStatement maintenance all as
syn keyword ecStatement	proc func init body tail

" ecstract functions
syn keyword ecFunction	any archiveSucceeded asCharacter asCounter asDate
syn keyword ecFunction	asFuzzyBoolean asHandle asInteger asNumber asParagraph
syn keyword ecFunction	asRowNumber associated asString average
syn keyword ecFunction  chequeMoney clock clockSnapshot close collating
syn keyword ecFunction	columnCount columnName comma configuration count crDr
syn keyword ecFunction	currencySymbol currentKey cursor customDate
syn keyword ecFunction	databaseName dataPath daysBetween dbDeleted
syn keyword ecFunction	dbFieldNumber dbFileNumber dbToInsert
syn keyword ecFunction	deletedByMaintenance deletedRow deletingByMaintenance
syn keyword ecFunction	displaying divide dmsContainsFile doubleLine drCr
syn keyword ecFunction	dynamicRow
syn keyword ecFunction	emailToDmsSubfolder ensureAppended exists
syn keyword ecFunction	exportSucceeded
syn keyword ecFunction	fileExists freeRecordCount fullDate
syn keyword ecFunction	hasGrandchildren hasLabel hasMinMax hasPdfPrinter height
syn keyword ecFunction	imported importSucceeded interactive isArchive
syn keyword ecFunction	isArchiveShell isDate isDisk isEmail isEmailToDms
syn keyword ecFunction	isEnquiry isEnquiryOnly isExcel isHardCopy isHTML
syn keyword ecFunction	isInteger isKompact isNA isNumber isOnScreen isOutput
syn keyword ecFunction	isPrinter isSQL isTable isV9 isWeb isWholeMonth
syn keyword ecFunction	isWorkingDay
syn keyword ecFunction	label length lf lineNumber linesLeft longDate longMoney
syn keyword ecFunction	lower
syn keyword ecFunction	matchesFilter matchesMinMax max min modified
syn keyword ecFunction	monthsBetween
syn keyword ecFunction	na noZero
syn keyword ecFunction	oldValue outputCopies OutputDmsSubfolder outputIsDms
syn keyword ecFunction	outputIsDmsOnly outputIsSplit outputIsSystemEmails
syn keyword ecFunction	outputPdfName
syn keyword ecFunction	pageFull pageHeight pageNumber pageWidth position
syn keyword ecFunction	promptY provider
syn keyword ecFunction	question
syn keyword ecFunction	random recordCount relativeDate renameFileSucceeded
syn keyword ecFunction	replace rgb round
syn keyword ecFunction	screenCode screenNumber screenTitle singleLine size
syn keyword ecFunction	soapFault soapSucceeded stripReturn substring
syn keyword ecFunction	targetTable timeAsString timeBetween today todaySnapshot
syn keyword ecFunction	traceLevel
syn keyword ecFunction	unpostedChargeDue upper upperOne userDepartment
syn keyword ecFunction	userEmail userFax userInitials userLevel userManager
syn keyword ecFunction	userName userPhone userReference
syn keyword ecFunction	vatAcronym
syn keyword ecFunction	webIsHTML webIsXML webParagraph webSucceeded
syn keyword ecFunction	wordPosition wrap
syn keyword ecFunction	zComma zCrDr zDrCr zero

" ecstract operators
" only bothering with word operators here, symbols can remain as is
syn keyword ecOperator	and containsWord ends from in mod not 
syn keyword ecOperator	or starts to
syn match ecOperator	" contains "
syn match ecOperator	" day[-+] "
syn match ecOperator	" workingDay[-+] "
syn match ecOperator	" month[-+] "
syn match ecOperator	" year[-+] "

" types
syn keyword ecType	boolean character choice counter date handle integer
syn keyword ecType	number paragraph set string tAuditNumber tMoney
syn keyword ecType	tReference tString
syn match ecType	" choice *of *custom "

" comments
syn match ecComment	";.*$" contains=@Spell
syn region ecComment	start=";(" end=");" contains=@Spell
syn match ecComment	"#.*$"
syn region ecComment	start="#(" end=")#"

" ecstract version comment
syn match ecVersion	"^; \[Ecstract 2\] *$"
syn match ecBadVersion	"^; \[Ecstract [^2].$"

" compiler helper comment
syn match ecCompile	"^;;compile *[^,]\+,[^,]\+,[^,]\+,[^,]\+$"

" kind of an experiment more than useful
"syn region ecComment	start="^;;compile " end="$" contains=ecCompHelp
"syn match ecCompHelp	"^;;compile " contained nextgroup=ecCompExe
"syn match ecCompExe	"[^,]*" contained nextgroup=ecCompAExe
"syn match ecCompAExe	"," contained nextgroup=ecCompEMS
"syn match ecCompEMS	"[^,]*" contained nextgroup=ecCompAEMS
"syn match ecCompAEMS	"," contained nextgroup=ecCompGroup
"syn match ecCompGroup	"[^,]*" contained nextgroup=ecCompAGroup
"syn match ecCompAGroup	"," contained nextgroup=ecCompApp
"syn match ecCompApp	"[^,]*" contained

" literals
syn match ecNumericLiteral	'\d\+'
syn match ecNumericLiteral	'[-+]\d\+'
syn match ecNumericLiteral	'\d\+\.\d\+'
syn match ecNumericLiteral	'[-+]\d\+\.\d\+'
" string literals need to be fairly late in the file as they override
" just about anything else
syn region ecStringLiteral	start="'" end="'" contains=@Spell
syn region ecStringLiteral	start="^ *|" end="$" contains=@Spell
syn keyword ecBooleanLiteral	true false

" built in variables
syn match ecVar		"result"	" this will need refining

" line continuation
syn match ecContinuation	'\\ *$'

" map to standard vim syntax types
hi def link ecStatement		Statement
hi def link ecFunction		Function
hi def link ecComment		Comment
hi def link ecStringLiteral	String
hi def link ecBooleanLiteral	Boolean
hi def link ecNumericLiteral	Number
hi def link ecComp		Preproc
hi def link ecOperator		Operator
hi def link ectype		Type
hi def link ecVar		Special
"hi def link ecVarConst		Identifier
"hi def link ecProcFunc		Identifier
hi def link ecContinuation	PreProc
hi def link ecVersion		PreProc
hi def link ecBadVersion	Error
hi def link ecCompile		PreProc

" these hilight defs just for compiler helper comment
"hi def link ecCompHelp		Comment
"hi def link ecCompExe		PreProc
"hi def link ecCompAExe		Comment	
"hi def link ecCompEMS		String
"hi def link ecCompAEMS		Comment
"hi def link ecCompGroup		PreProc
"hi def link ecCompAGroup	Comment
"hi def link ecCompApp		String

let b:current_syntax = "ecstract3"

