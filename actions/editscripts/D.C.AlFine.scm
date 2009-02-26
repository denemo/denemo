;;; tweak values of D.C. Al fine indication, d-x and d-y are set by dragging in printview area.
;;D.C.AlFine edit choices

(let ((choice #f) (edit-text #f) (newtext "") (place-above ""))
  (begin
    (set! edit-text (lambda ()
		      (begin
			(set! newtext (d-GetUserInput "Replacing Text" "Give new text" "D.C.AlFine" ))
			(d-DirectivePut-chord-postfix   "D.C.AlFine"  (string-append "^\\markup \\italic \\bold {" newtext "}"))
			(d-DirectivePut-chord-display  "D.C.AlFine" newtext)
			)))

 (set! place-above (lambda (above)
		      (begin
			(set! newtext (d-DirectiveGet-chord-postfix  "D.C.AlFine"))
			(cond
			 (above (string-set! newtext 0 #\^))
			 ((not above) (string-set! newtext 0 #\_)))
			(d-DirectivePut-chord-postfix   "D.C.AlFine" newtext))))


    (set! choice (d-GetOption "Place above staff\0Place below staff\0Edit offset\0Edit text\0"))
    (cond
     ((boolean? choice)
      (d-WarningDialog "Operation cancelled"))

     ((equal? choice "Place above staff")
      (place-above #t))
     ((equal? choice "Place below staff")
      (place-above #f))

     ((equal? choice "Edit text")
      (edit-text))	
     ((equal? choice "Edit offset")
      (ExtraOffset "TextScript")))
    (d-RefreshDisplay)))
