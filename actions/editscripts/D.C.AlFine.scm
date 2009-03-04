;;; tweak values of D.C. Al fine indication
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


    (set! choice (d-GetOption (string-append cue-PlaceAbove stop  cue-PlaceBelow stop cue-SetRelativeFontSize stop cue-SetPadding stop cue-OffsetPositionAll  stop cue-EditText stop)))

    (cond
     ((boolean? choice)
      (d-WarningDialog "Operation cancelled"))

     ((equal? choice cue-PlaceAbove)
      (place-above #t))
     ((equal? choice cue-PlaceBelow)
      (place-above #f))
     ((equal? choice cue-SetRelativeFontSize)
      (SetRelativeFontSize "TextScript"     ))
     ((equal? choice  cue-EditText)
      (edit-text))
     ((equal? choice cue-SetPadding)
      (SetPadding "TextScript"))	
     ((equal? choice cue-OffsetPositionAll)
      (ExtraOffset "TextScript")))
    (d-RefreshDisplay)))
