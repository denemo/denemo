;;; tweak values of D.C. Al fine indication, d-x and d-y are set by dragging in printview area.

(let ((choice #f) (edit-text #f) (newtext ""))
      (begin
	(set! edit-text (lambda ()
			  (begin
			    (set! newtext (d-GetUserInput "Replacing Text" "Give new text" "D.C.AlFine" ))
			    (d-DirectivePut-chord-postfix   "D.C.AlFine"  (string-append "^\\markup \\italic \\bold {" newtext "}"))
			    (d-DirectivePut-chord-display  "D.C.AlFine" newtext)
			    )))
	  (set! choice (d-GetOption "Edit position\0Edit text\0"))
	  (cond
	   ((boolean? choice)
	    (d-WarningDialog "Operation cancelled"))
	   ((equal? choice "Edit text")
	    (edit-text))	
	   ((equal? choice "Edit position")
	    (ExtraOffset "TextScript")))
	  (d-RefreshDisplay)))
