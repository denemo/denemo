;;DCAlFine
(let ((tag "DCAlFine"))
(if (d-Directive-chord? tag)
	(begin
	 (let ((choice #f) (edit-text #f) (newtext "") (place-above ""))
	  (begin
   		 (set! edit-text (lambda ()
		      (begin
			(set! newtext (d-GetUserInput (_ "Replacing Text") (_ "Give new text") (_ "D.C. Al Fine") ))
			(d-DirectivePut-chord-postfix   tag  (string-append "^\\markup \\italic \\bold {" newtext "}"))
			(d-DirectivePut-chord-display  tag newtext)
			)))
 		(set! place-above (lambda (above)
		      (begin
			(set! newtext (d-DirectiveGet-chord-postfix tag))
			(cond
			 (above (string-set! newtext 0 #\^))
			 ((not above) (string-set! newtext 0 #\_)))
			(d-DirectivePut-chord-postfix   tag newtext))))
    		(set! choice (d-GetOption (string-append cue-PlaceAbove stop cue-PlaceBelow stop cue-SetRelativeFontSize stop cue-EditText stop 
						cue-RestorePosition stop cue-Delete stop cue-Advanced stop)))
   		 (cond
    			 ((boolean? choice)
      			(d-WarningDialog "Operation cancelled"))

    			 ((equal? choice cue-PlaceAbove)
     				 (place-above #t))
     			((equal? choice cue-PlaceBelow)
      				(place-above #f))
    			 ((equal? choice cue-SetRelativeFontSize)
      				(SetRelativeFontSize tag ))
     			((equal? choice  cue-EditText)
      				(edit-text))
     			((equal? choice cue-RestorePosition)
      				(d-DirectivePut-chord-prefix tag ""))	
     			((equal? choice cue-Delete)
      			(d-DirectiveDelete-chord tag))
      		((equal? choice cue-Advanced)
      			(d-DirectiveTextEdit-chord tag)))
      			(d-SetSaved #f)
    			(d-RefreshDisplay))))
	(begin
	   (if (Music?)
	   	(begin
				(d-DirectivePut-chord-display tag  "D.C. al fine" )
				(d-DirectivePut-chord-postfix tag  "^\\markup \\italic \\bold {D. C. al fine}")
				(d-DirectivePut-chord-minpixels tag 20)
				(d-SetSaved #f)
				(d-RefreshDisplay))
		(begin
			(d-WarningDialog (_ "No note or rest at cursor")))))))