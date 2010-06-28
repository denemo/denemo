
Function create_shortcuts
	;; Start menu
	CreateDirectory "$SMPROGRAMS\LilyPond"
	CreateShortCut "$SMPROGRAMS\LilyPond\LilyPond.lnk" \
		"$INSTDIR\usr\bin\lilypond-windows.exe" "-dgui" \
		"$INSTDIR\usr\bin\lilypond-windows.exe" 0 SW_SHOWMINIMIZED
	CreateShortCut "$SMPROGRAMS\LilyPond\LilyPond Tutorial.lnk" \
		"http://lilypond.org/tutorial" "" \
		"firefox.exe" 0
	CreateShortCut "$SMPROGRAMS\LilyPond\LilyPond Website.lnk" \
		"http://lilypond.org/" "" \
		"firefox.exe" 0
	CreateShortCut "$SMPROGRAMS\LilyPond\Music in Mutopia.lnk" \
		"http://www.mutopiaproject.org" "" \
		"$INSTDIR\usr\bin\lilypond-windows.exe" 1
	CreateShortCut "$SMPROGRAMS\LilyPond\Examples.lnk" \
		"$INSTDIR\usr\share\doc\lilypond\input" "" \
		"$INSTDIR\usr\bin\lilypond-windows.exe" 1
	CreateShortCut "$SMPROGRAMS\LilyPond\Uninstall.lnk" \
		"$INSTDIR\uninstall.exe" "" "$INSTDIR\uninstall.exe" 0


	;; Desktop
	ClearErrors
	ReadRegStr $R0 HKLM \
		"SOFTWARE\Microsoft\Windows NT\CurrentVersion" CurrentVersion
	IfErrors dos windows
dos:
	CreateShortCut "$DESKTOP\LilyPond.lnk" "" \
		"$INSTDIR\usr\bin\lilypond-windows.bat" \
		"$INSTDIR\usr\bin\lilypond-windows.exe" 0 SW_SHOWMINIMIZED
	Goto exit
windows:
	CreateShortCut "$DESKTOP\LilyPond.lnk" \
		"$INSTDIR\usr\bin\lilypond-windows.exe" "-dgui" \
		"$INSTDIR\usr\bin\lilypond-windows.exe" 0 SW_SHOWMINIMIZED
		
exit:
FunctionEnd


Function registry_lilypond
	ReadRegStr $R0 HKLM "${ENVIRON}" "PATH"
	WriteRegExpandStr HKLM "${ENVIRON}" "PATH" "$R0;$INSTDIR\usr\bin"

	WriteRegStr HKCR ".ly" "" "LilyPond"
	WriteRegStr HKCR ".ly" "LilyPond" "LilyPond"
	WriteRegStr HKCR ".ly" "Content Type" "text/lilypond-source"

;;ly_icon:
	WriteRegStr HKCR "LilyPond" "DefaultIcon" ""
	WriteRegStr HKCR "LilyPond\DefaultIcon" "" \
		    "$INSTDIR\usr\bin\lilypond-windows.exe,1"

;;ly_open:
	ReadRegStr $R0 HKCR "LilyPond\shell\open\command" ""
	;;StrCmp $R0 "" 0 ly_edit
	WriteRegStr HKCR "LilyPond\shell" "" "open"
	WriteRegExpandStr HKCR "LilyPond\shell\open\command" "" '"$EDITOR" "%1"'

;;ly_edit:
	ReadRegStr $R0 HKCR "LilyPond\shell\edit\command" ""
	;;StrCmp $R0 "" 0 ly_generate
	WriteRegStr HKCR "LilyPond\shell" "" "edit"
	WriteRegStr HKCR "LilyPond\shell\edit" "" "&Edit source..."
	WriteRegExpandStr HKCR "LilyPond\shell\edit\command" "" '"$EDITOR" "%1"'

;;ly_generate:
	ReadRegStr $R0 HKCR "LilyPond\shell\generate\command" ""
	;;StrCmp $R0 "" 0 ly_auto_file
	WriteRegStr HKCR "LilyPond\shell" "" "generate"
	WriteRegStr HKCR "LilyPond\shell\generate" "" "&Generate PDF ..."
	WriteRegExpandStr HKCR "LilyPond\shell\generate\command" "" \
			  '"$INSTDIR\usr\bin\lilypond-windows.exe" -dgui "%1"'

;; what's the difference between ly_auto_file and shell/open/command?
;;ly_auto_file:
	ReadRegStr $R0 HKCR "ly_auto_file\shell\open\command" ""
	;;StrCmp $R0 "" 0 ly_generate
	WriteRegStr HKCR "ly_auto_file\shell" "" "open"
	WriteRegExpandStr HKCR "ly_auto_file\shell\open\command" "" '"$EDITOR" "%1"'

;;textedit_open:
	WriteRegStr HKCR "textedit" "URL protocol" ""
	ReadRegStr $R0 HKCR "textedit\shell\open\command" ""
	;;StrCmp $R0 "" 0 exit
	WriteRegStr HKCR "textedit\shell" "" "open"
	WriteRegExpandStr HKCR "textedit\shell\open\command" "" '"$INSTDIR\usr\bin\guile.exe" -e main -s "$INSTDIR\usr\bin\lilypond-invoke-editor.scm" "%1"'
;;exit:
FunctionEnd



Function postinstall_lilypad
	StrCpy $0 "$INSTDIR\usr\bin\lilypad"
	CopyFiles /silent "$0.exe" "$0-unicode.exe"
	ClearErrors
	ReadRegStr $R0 HKLM \
		"SOFTWARE\Microsoft\Windows NT\CurrentVersion" CurrentVersion
	IfErrors dos exit
dos:
	CopyFiles /silent "$0-ascii.exe" "$0.exe"
exit:	
FunctionEnd

Function postinstall_lilypond
	StrCpy $0 "$INSTDIR\usr\bin\variables.sh"
	${SubstituteAtVariables} "$0.in" "$0"

	# use console version for gui too
	StrCpy $0 "$INSTDIR\usr\bin\lilypond"
	ClearErrors
	ReadRegStr $R0 HKLM \
		"SOFTWARE\Microsoft\Windows NT\CurrentVersion" CurrentVersion
	IfErrors dos exit
dos:
	CopyFiles /silent "$0-windows.exe" "$0-windows-orig.exe"
	CopyFiles /silent "$0.exe" "$0-windows.exe"
	StrCpy $0 "$INSTDIR\usr\bin\lilypond-windows.bat"
	${SubstituteAtVariables} "$0.in" "$0"

exit:	
FunctionEnd

Function postinstall_lilypond2
	StrCpy $0 "$INSTDIR\usr\bin\variables.sh"
	${SubstituteAtVariables} "$0.in" "$0"

	# use console version for gui too
	StrCpy $0 "$INSTDIR\usr\bin\lilypond"
	ClearErrors
	IfErrors dos exit
dos:
	CopyFiles /silent "$0-windows.exe" "$0-windows-orig.exe"
	CopyFiles /silent "$0.exe" "$0-windows.exe"
	StrCpy $0 "$INSTDIR\usr\bin\lilypond-windows.bat"
	${SubstituteAtVariables} "$0.in" "$0"

exit:	
FunctionEnd


Function registry_guile
	ReadRegStr $R0 HKLM "${ENVIRON}" "PATHEXT"
 	${StrLoc} $0 $R0 ".SCM;" >
	StrCmp $0 "" 0 scm_done
	WriteRegStr HKLM "${ENVIRON}" "PATHEXT" ".SCM;$R0"

scm_done:
	WriteRegStr HKCR ".scm" "" "GUILE"
	WriteRegStr HKCR ".scm" "GUILE" "GUILE"
	WriteRegStr HKCR ".scm" "Content Type" "text/x-guile"

	ReadRegStr $R0 HKCR "GUILE\shell\open\command" ""
	;;StrCmp $R0 "" 0 exit
	WriteRegStr HKCR "GUILE\shell" "" "open"
	WriteRegExpandStr HKCR "GUILE\shell\open\command" "" '"$INSTDIR\usr\bin\guile.exe" -e main -s "%1"'
exit:
FunctionEnd

Function registry_python
	ReadRegStr $R0 HKLM "${ENVIRON}" "PATHEXT"
 	${StrLoc} $0 $R0 ".PY;" >
	StrCmp $0 "" 0 py_done
	WriteRegStr HKLM "${ENVIRON}" "PATHEXT" ".PY;$R0"

py_done:
	WriteRegStr HKCR ".py" "" "Python"
	WriteRegStr HKCR ".py" "Python" "Python"
	WriteRegStr HKCR ".py" "Content Type" "text/x-python"

;;py_open:
	ReadRegStr $R0 HKCR "Python\shell\open\command" ""
	;;StrCmp $R0 "" 0 py_auto_file
	WriteRegStr HKCR "Python\shell" "" "open"
	# %1 is the PYTHON command, so must be quoted bo the space
	WriteRegExpandStr HKCR "Python\shell\open\command" "" '"$INSTDIR\usr\bin\python.exe" "%1" %2 %3 %4 %5 %6 %7 %8 %9'

;;py_auto_file:
	ReadRegStr $R0 HKCR "py_auto_file\shell\open\command" ""
	;;StrCmp $R0 "" 0 py_end
	WriteRegStr HKCR "py_auto_file\shell" "" "open"
	# %1 is the PYTHON command, so must be quoted bo the space
	WriteRegExpandStr HKCR "py_auto_file\shell\open\command" "" '"$INSTDIR\usr\bin\python.exe" "%1" %2 %3 %4 %5 %6 %7 %8 %9'
;;py_end:	
FunctionEnd


Function find_editor
	StrCpy $R0 "$WINDIR\emacs.exe"
	ifFileExists $R0 exit
	StrCpy $R0 $WINDIR\jedit.bat
	ifFileExists $R0 0 lilypad
	StrCpy $R0 "$R0 %(file)s +line:%(line)s"
lilypad:
	StrCpy $R0 "$INSTDIR\usr\bin\lilypad.exe"
	ifFileExists $R0 exit
	StrCpy $R0 "$WINDIR\wordpad.exe"
	ifFileExists $R0 exit
	StrCpy $R0 "$WINDIR\NOTEPAD.EXE"
exit:
	StrCpy $EDITOR "$R0" 0 0 
FunctionEnd
