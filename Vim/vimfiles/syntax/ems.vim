" Vim syntax file for ems file

" Quit when a syntax file was already loaded
if exists("b:current_syntax")
  finish
endif

syn keyword nvpName Version Code Data ArchiveData Software PdfTimeout Title
syn keyword nvpName EmailMechanism OutlookTasksAsEmails SmtpPort PageSeparator
syn keyword nvpName SQL SqlPlatform SqlHost SqlDataSet SqlAutoCommit
syn keyword nvpName WindowsAuthentication
syn keyword nvpName MultiUserLogInterval MultiUserLogCallStackDepth
syn keyword nvpName WordLinkVisible PdfViaWord
syn keyword nvpName Redemption
syn match nvpName	"Ecs Classic Keystrokes="me=e-1
syn match nvpName	"Hash As Pound="me=e-1
syn match nvpName	"Use PrnCh1="me=e-1
syn match emsSection	"^\[.*\]"
syn match nvpValue	"=.*"ms=s+1

" map to standard vim syntax types
hi def link emsSection		Statement
hi def link nvpName		Identifier
hi def link nvpValue		Constant

let b:current_syntax = "ems"
