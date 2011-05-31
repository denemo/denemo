;;;; denemo.nsi -- Denemo installer script for Microsoft Windows
;;;; (c) 2005--2009
;;;; Jan Nieuwenhuizen <janneke@gnu.org>
;;;; Han-Wen Nienhuys <janneke@gnu.org>
;;;; licence: GNU GPL

;;;; FIXME: mostly cut-and paste from lilypond.nsi [installer.nsi]
;; For quick [wine] test runs
;; !define TEST "1"


;;; substitutions

!define ENVIRON "SYSTEM\CurrentControlSet\Control\Session Manager\Environment"

!define UNINSTALL \
	"Software\Microsoft\Windows\CurrentVersion\Uninstall\${PRETTY_NAME}"
!define USER_SHELL_FOLDERS \
	"Software\Microsoft\Windows\CurrentVersion\Explorer\User Shell Folders"

Var "EDITOR"
Var "UP_DESKTOP"

!define UninstLog "files.txt"
Var UninstLog

; Uninstall log file missing.
LangString UninstLogMissing ${LANG_ENGLISH} "${UninstLog} not found.$\r$\nCannot uninstall."

!include "substitute.nsh"
${StrLoc}
${UnStrLoc}

;;SetCompressor lzma  ; very slow
;;SetCompressor zlib
SetCompressor bzip2  ;;

Name "${PRETTY_NAME}"

Caption "${PRETTY_NAME} 0.9.1"
BrandingText "${PRETTY_NAME} installer v1.0"


InstallDir $PROGRAMFILES\${PRETTY_NAME}
InstallDirRegKey HKLM "Software\${PRETTY_NAME}" "Install_Dir"

CRCCheck on
XPStyle on
InstallColors /windows

BGGradient 000000 E8FFE8 FFFFFF

;; Use Finish iso Close for the [close button text]
;; Although nothing happens after Close, experienced Windows users feel
;; much more with "Finish" than with Close.
MiscButtonText Back Next Cancel Finish

LicenseText "Conditions for redistributing ${PRETTY_NAME}" "Next"
LicenseData "${ROOT}\license\${NAME}"
LicenseForceSelection off

Page license

;; FIXME: the installer will crash on File /r commands if Page
;; directory is not used.
Page directory

Page components

;; Put a note to look at the Help page of the website on the
;; window when the install is completed
CompletedText "Install completed.  Please see $INSTDIR\usr\bin\${CANARY_EXE}."
Page instfiles

UninstPage uninstConfirm
UninstPage instfiles

Section "${PRETTY_NAME} (required)"
	;; always generate install log
	Logset on

silent:
	IfFileExists $INSTDIR\usr\bin\${CANARY_EXE}.exe no_overwrite_error fresh_install
no_overwrite_error:
	MessageBox MB_OK "Previous version of ${PRETTY_NAME} found$\r$\nDelete the old version in $INSTDIR manually first (fast). Or Uninstall the old version first. (Broken Slow!)"
	Abort "Previous version of ${PRETTY_NAME} found$\r$\nDelete the old version in $INSTDIR manually first (fast). Or Uninstall the old version first. (Broken Slow!)"

fresh_install:
	SetOverwrite on
	AllowSkipFiles on
	SetOutPath $INSTDIR

	File /r "${ROOT}\usr"
	File /r "${ROOT}\license"
	File /r "${ROOT}\files.txt"

	WriteUninstaller "uninstall.exe"
	CreateDirectory "$INSTDIR\usr\bin"

	;; Use tested lilypad for now
	StrCpy $EDITOR "$INSTDIR\usr\bin\lilypad.exe"
	Call registry_installer
	;;Call registry_path
	;;Call registry_lilypond

	;; FIXME: these postinstall things should be part of their
	;; respective packages once we have min-apt or Cygwin's
	;; setup.exe in place.

	Call postinstall_lilypond
	;;Call postinstall_lilypad
	;;Call postinstall_denemo
SectionEnd

Function registry_path
	ReadRegStr $R0 HKLM "${ENVIRON}" "PATH"
	WriteRegExpandStr HKLM "${ENVIRON}" "PATH" "$R0;$INSTDIR\usr\bin"
FunctionEnd

;; copy & paste from the NSIS code examples
Function un.install_installed_files
 IfFileExists "$INSTDIR\${UninstLog}" +3
  MessageBox MB_OK|MB_ICONSTOP "$(UninstLogMissing)"
   Abort

 Push $R0
 Push $R1
 Push $R2
 SetFileAttributes "$INSTDIR\${UninstLog}" NORMAL
 FileOpen $UninstLog "$INSTDIR\${UninstLog}" r
 StrCpy $R1 0

 GetLineCount:
  ClearErrors
   FileRead $UninstLog $R0
   IntOp $R1 $R1 + 1
   IfErrors 0 GetLineCount

 LoopRead:
  FileSeek $UninstLog 0 SET
  StrCpy $R2 0
  FindLine:
   FileRead $UninstLog $R0
   IntOp $R2 $R2 + 1
   StrCmp $R1 $R2 0 FindLine

   StrCpy $R0 "$INSTDIR\$R0" -2
   IfFileExists "$R0\*.*" 0 +3
    RMDir $R0  #is dir
   Goto +3
   IfFileExists "$R0" 0 +2
    Delete "$R0" #is file

  IntOp $R1 $R1 - 1
  StrCmp $R1 0 LoopDone
  Goto LoopRead
 LoopDone:
 FileClose $UninstLog

 Pop $R2
 Pop $R1
 Pop $R0

FunctionEnd
;; end copy & paste


Section "Uninstall"
	ifSilent 0 silent
	Logset on

silent:
	DeleteRegKey HKLM SOFTWARE\${PRETTY_NAME}
	DeleteRegKey HKLM "${UNINSTALL}"

	DeleteRegKey HKCR "${PRETTY_NAME}" ""


	ReadRegStr $R0 HKLM "${ENVIRON}" "PATH"
	${UnStrLoc} $0 $R0 "$INSTDIR\usr\bin;" >

path_loop:
	StrCmp $0 "" path_done
	StrLen $1 "$INSTDIR\usr\bin;"
	IntOp $2 $0 + $1
	StrCpy $3 $R0 $0 0
	StrCpy $4 $R0 10000 $2
	WriteRegExpandStr HKLM "${ENVIRON}" "PATH" "$3$4"
	ReadRegStr $R0 HKLM "${ENVIRON}" "PATH"
	${UnStrLoc} $0 $R0 "$INSTDIR\usr\bin;" >
	StrCmp $0 "" path_done path_loop

path_done:
	;;call un.install_denemo_ttf
	call un.install_installed_files

	;; Remove shortcuts, if any
	SetShellVarContext all
	Delete "$SMPROGRAMS\${PRETTY_NAME}\*.*"
	Delete "$DESKTOP\${PRETTY_NAME}.lnk"
	RMDir "$SMPROGRAMS\${PRETTY_NAME}"

	SetShellVarContext current
	Delete "$SMPROGRAMS\${PRETTY_NAME}\*.*"
	Delete "$DESKTOP\${PRETTY_NAME}.lnk"
	RMDir "$SMPROGRAMS\${PRETTY_NAME}"

	;; Remove directories used
	RMDir "$SMPROGRAMS\${PRETTY_NAME}"
	RMDir "$INSTDIR\usr\bin"
	RMDir "$INSTDIR\usr\"
	Delete "$INSTDIR\uninstall.exe"
	Delete "$INSTDIR\files.txt"

	RMDir "$INSTDIR"
SectionEnd

Function registry_installer
	WriteRegStr HKLM "SOFTWARE\${PRETTY_NAME}" "Install_Dir" "$INSTDIR"
	WriteRegStr HKLM "${UNINSTALL}" "DisplayName" "${PRETTY_NAME}"
	WriteRegStr HKLM "${UNINSTALL}" "UninstallString" '"$INSTDIR\uninstall.exe"'
	WriteRegDWORD HKLM "${UNINSTALL}" "NoModify" 1
	WriteRegDWORD HKLM "${UNINSTALL}" "NoRepair" 1
FunctionEnd

;; Optional section (can be disabled by the user)
Section "Start Menu Shortcuts"
	;; First install for all users, if anything fails, install
	;; for current user only.
	ClearErrors

	;; The OutPath specifies the CWD of the command.  For desktop
	;; shortcuts, set to a string that expands to the desktop folder
	;; of the user who runs LilyPond.
	ReadRegStr $R0 HKCU "${USER_SHELL_FOLDERS}" "Desktop"
	SetOutPath '"$R0"'
	SetShellVarContext all

	;; Working directory: %USERPROFILE%\<locale's-desktop-folder-name>,
	;; but that string is not expanded.

	;; Let's see what happens when outputting to the shared desktop.
	SetOutPath "$DESKTOP"
	Call denemo_create_shortcuts

	;; That also did not work, often the other users do no write access
	;; there.

	;; If no write access for all, delete common stuff and opt for
	;; install for current user only.
	IfErrors 0 exit
	Delete "$DESKTOP\Denemo.lnk"
	Delete "$SMPROGRAMS\Denemo\*.*"
	RMDir "$SMPROGRAMS\Denemo"

	;; $DESKTOP should expand to the same location as the outpath above,
	;; but nsis may handle anomalies better.

current_user:
	SetShellVarContext current
	SetOutPath "$DESKTOP"
	Call denemo_create_shortcuts

exit:
	SetShellVarContext current
	SetOutPath $INSTDIR
SectionEnd

!include "lilypond-prepost.nsh"

Function denemo_create_shortcuts
	;; Start menu
	CreateDirectory "$SMPROGRAMS\Denemo"
	CreateShortCut "$SMPROGRAMS\Denemo\Denemo.lnk" \
		"$INSTDIR\usr\bin\denemo.exe" "" \
		"$INSTDIR\usr\bin\denemo.exe" 0 SW_SHOWNORMAL
	CreateShortCut "$SMPROGRAMS\Denemo\Denemo Website.lnk" \
		"http://www.denemo.org/" "" \
		"firefox.exe" 0
	;;CreateShortCut "$SMPROGRAMS\Denemo\LilyPond.lnk" \
	;;	"$INSTDIR\usr\bin\lilypond-windows.exe" "-dgui" \
	;;	"$INSTDIR\usr\bin\lilypond-windows.exe" 0 SW_SHOWMINIMIZED
	;;CreateShortCut "$SMPROGRAMS\Denemo\LilyPond Website.lnk" \
	;;	"http://lilypond.org/" "" \
	;;	"firefox.exe" 0
	;;CreateShortCut "$SMPROGRAMS\Denemo\Music in Mutopia.lnk" \
	;;		"http://www.mutopiaproject.org" "" \
	;;		"$INSTDIR\usr\bin\lilypond-windows.exe" 1
	;; CreateShortCut "$SMPROGRAMS\Denemo\Examples.lnk" \
	;; "$INSTDIR\usr\share\doc\lilypond\input" "" \
	;; "$INSTDIR\usr\bin\lilypond-windows.exe" 1
	CreateShortCut "$SMPROGRAMS\Denemo\Uninstall.lnk" \
		"$INSTDIR\uninstall.exe" "" "$INSTDIR\uninstall.exe" 0


	;; Desktop
	ClearErrors
	ReadRegStr $R0 HKLM \
		"SOFTWARE\Microsoft\Windows NT\CurrentVersion" CurrentVersion
	IfErrors dos windows
dos:
	CreateShortCut "$DESKTOP\Denemo.lnk" "" \
		"$INSTDIR\usr\bin\denemo.bat" \
		"$INSTDIR\usr\bin\denemo.exe" 0 SW_SHOWNORMAL
	Goto exit
windows:
	CreateShortCut "$DESKTOP\Denemo.lnk" \
		"$INSTDIR\usr\bin\denemo.exe" "" \
		"$INSTDIR\usr\bin\denemo.exe" 0 SW_SHOWNORMAL
	;; CreateShortCut "$DESKTOP\LilyPond.lnk" \
	;;	"$INSTDIR\usr\bin\lilypond-windows.exe" "-dgui" \
	;;	"$INSTDIR\usr\bin\lilypond-windows.exe" 0 SW_SHOWMINIMIZED
		
exit:
FunctionEnd

!include "FontName.nsh"
!include "FontReg.nsh"

;Function postinstall_denemo
	;CopyFiles /silent "$INSTDIR\usr\share\fonts\truetype\denemo\Denemo.ttf" "$WINDIR\Fonts\Denemo.ttf"
	;StrCpy $FONT_DIR "$WINDIR\Fonts"
	;!insertmacro InstallTTFFont "${ROOT}\usr\share\fonts\truetype\denemo\Denemo.ttf"
	;ClearErrors
;FunctionEnd

;;Function un.install_denemo_ttf
; Call must be used with function names starting with "un." in the uninstall section.
; Usage: Call function_name | [:label_name]
; Error in macro GetFileNameCall on macroline 2
; Error in macro RemoveTTFFont on macroline 9
; Error in script "/home/janneke/vc/gub/target/mingw/installer/denemo--dbdir/denemo.nsi" on line 331 -- aborting creation process
;	!insertmacro RemoveTTFFont "Denemo.ttf"
;;	Delete "$WINDIR\Fonts\Denemo.ttf"
;;	ClearErrors
;;FunctionEnd
