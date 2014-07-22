;;;; TransposeScorePrint
(define-once Transpose::Interval "c ees")

(let ((text #f) (global-tag "GlobalTranspose")(tag "TransposeOnPrint"))
(if (and TransposeScorePrint::params (not (equal?  TransposeScorePrint::params "edit")))
    (set! Transpose::Interval TransposeScorePrint::params)
    (set! Transpose::Interval (d-GetUserInput (_ "Set Transpose Interval") (_ "Give Interval to transpose by as two note names, 
     for example \"c g\" means transpose 5th up.
    Note names are in Dutch!!! a,b,c ... are the same but
    \"es\" = flat, so e.g. bes means b-flat
    \"is\" = sharp so e.g fis means f-sharp
    Use commas for octave(s) down, 
    single-quotes for octave(s) up
    e.g. c c' means octave up.
    You do not have to start with c
    e.g. d e means a tone higher.
    ") Transpose::Interval)))
(if Transpose::Interval
  (let ((choice (RadioBoxMenu
                     (cons "Global (includes quoted music)"   'global)   
                        (cons "Main Score Only" 'score))))
              (d-DirectiveDelete-score "TransposeScorePrint") ;;;get rid of old style transpose directive      
               (d-DirectiveDelete-score tag)
               (d-DirectiveDelete-score global-tag)
              (case choice
         			 ((global)      
         			 	 (d-DirectivePut-score-override global-tag  DENEMO_OVERRIDE_AFFIX)
         			 	 (d-DirectivePut-score-prefix global-tag   (string-append     "\nDenemoGlobalTranspose = #(define-music-function (parser location arg)(ly:music?) #{\\transpose "
         			 Transpose::Interval "#arg #})\n")) (d-DirectivePut-score-postfix tag  "\\DenemoGlobalTranspose "))
  	  			(else
  	  			    	 (d-DirectivePut-score-postfix tag (string-append  "\\transpose " Transpose::Interval " "))))
	  (set! text (string-append  "Print transposed:  " Transpose::Interval " ")) 
	  (d-DirectivePut-score-display tag text)
	  (d-DirectivePut-score-override tag DENEMO_OVERRIDE_GRAPHIC)
	  (d-SetSaved #f))))
