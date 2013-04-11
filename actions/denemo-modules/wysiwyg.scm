;;; routines for manipulating the evince widget's display for wysiwyg operations on the print view.

;;returns the nth line counting from 0, returns "" if not enough lines in text.
(define (GetNthLine text n)
	(let ((thelist (string-split text #\newline)))
		(if (> (length thelist) n)
		(list-ref thelist n)
		"")))


;;BeamCount
(define (BeamCount direction n)
	(define tag (string-append "Beam" direction))
	(if n
		(begin
			(set! n (number->string n))
			(d-DirectivePut-chord-prefix tag (string-append "\\set stem" direction "BeamCount = #" n " "))
			(d-DirectivePut-chord-override tag DENEMO_OVERRIDE_AFFIX)
			(d-DirectivePut-chord-display tag  (string-append (if (equal? direction "Left") "[" "]") n )))
		(d-DirectiveDelete-chord tag))
	(d-RefreshDisplay)
	(d-SetSaved #f))


;;ChopBeaming
(define (ChopBeaming n)
	(define (chop-beam-right)
		(if (and (Music?) (not (Rest?)))
			(if (> (d-GetNoteBaseDuration) (+ 2 0))
				(begin
					(BeamCount "Left" n)
					#t)
			(if (and (not (Music?)) (d-MoveCursorRight))
				(chop-beam-right)
				#f))
			(if (and (not (Music?)) (d-MoveCursorRight))
				(chop-beam-right)
				#f)))
 (if (and (Music?) (not (Rest?)) (> (d-GetNoteBaseDuration) (+ 2 0)))
	(begin
		(d-PushPosition)
		(d-MoveCursorRight)
		(if (chop-beam-right)
			(begin
				(d-PopPosition)
				(BeamCount "Right" n))
			(d-PopPosition)))))


	
(define (TweakRelativeOffset tag offsetx offsety)
		(define newvalue (ChangeOffset (d-DirectiveGet-standalone-prefix tag) "-\\tweak #'extra-offset #'("  ")" (cons offsetx offsety)))
	(d-DirectivePut-standalone-prefix tag newvalue))
	
(define (TweakRelativeFontSize tag size)
		(define newvalue (ChangeValue (d-DirectiveGet-standalone-prefix tag) "-\\tweak #'font-size #" " " size))
	(d-DirectivePut-standalone-prefix tag newvalue))
	
; ExtraAmount
;; the parameter "what" is the LilyPond grob that is being tweaked - it may not be the tag of the DenemoDirective that is being edited
;; property is the (two values - a pair) lilypond property being altered
(define* (ExtraAmount what property tag #:optional (type "chord") (context "") (offset '(0 . 0)) (override #f))
  (let ((oldstr #f) (start "") (end "") (get-command d-DirectiveGet-chord-prefix)  (put-command d-DirectivePut-chord-prefix) (override-command d-DirectivePut-chord-override))
  ;(disp "Entered with " offset "and " type " and " context " ok")
    (cond
     ((string=? type "chord")
      (begin (set! get-command d-DirectiveGet-chord-prefix)
	     (set! put-command d-DirectivePut-chord-prefix)
	     (set! override-command d-DirectivePut-chord-override)
	     ))
     ((string=? type "note")
      (begin (set! get-command d-DirectiveGet-note-prefix)
	     (set! put-command d-DirectivePut-note-prefix)
	     (set! override-command d-DirectivePut-note-override)))
     ((string=? type "standalone")
      (begin (set! get-command d-DirectiveGet-standalone-prefix)
	     (set! put-command d-DirectivePut-standalone-prefix)
	     (set! override-command d-DirectivePut-standalone-override)))
     )

    (set! oldstr (get-command tag))
    (if (equal? oldstr "")
	(set! oldstr #f))
		;(disp "The old prefix was " oldstr " with " tag " from running " get-command " ok???")
    (set! start (string-append "\\once \\override " context what " #'" property " = #'("))
    (set! end ")")
    (if override
			(override-command tag override))
    (put-command tag (ChangeOffset oldstr start end offset))))

;;    
(define* (ExtraOffset tag what #:optional (type "chord") (context "") (offset '(0 . 0)) (override #f))
	(ExtraAmount what "extra-offset" tag type context offset override))

;;    
(define* (AlterPositions tag what  #:optional (type "chord") (context "") (positions '(0 . 0)) (override #f))
	(ExtraAmount what "positions" tag type context positions override))


; SetRelativeFontSize
(define* (SetRelativeFontSize what #:optional (type "chord") (context "") (override #f))
  (SetValue ChangeRelativeFontSize " #'font-size = #" what type context override))

; SetPadding
(define* (SetPadding what  #:optional (type "chord") (context ""))
  (SetValue ChangePad " #'padding = #" what type context))
  


; SetValue
(define* (SetValue change-func change-str  what  #:optional (type "chord") (context "") (override #f))
  (let ((tag "") (oldstr #f) (start "") (end "") (pad "")  (override-command d-DirectivePut-chord-override) (get-command d-DirectiveGet-chord-prefix) (put-command d-DirectivePut-chord-prefix))
    (cond
     ((string=? type "note")
      (begin (set! get-command d-DirectiveGet-note-prefix)
       (set! override-command d-DirectivePut-note-override)
	     (set! put-command d-DirectivePut-note-prefix)))
     ((string=? type "standalone")
      (begin (set! get-command d-DirectiveGet-standalone-prefix) 
       (set! override-command d-DirectivePut-standalone-override)
	     (set! put-command d-DirectivePut-standalone-prefix)))
     )
    (set! start (string-append "\\once \\override " context what change-str))
    (set! end " ")
    (set! tag what)
    (set! oldstr (get-command tag))
    (if (equal? oldstr "")
			(set! oldstr #f))
    (put-command tag (change-func oldstr start end))
    (if override
			(override-command override))))

; ChangeOffset
;; e.g.  (define prefixstring      "\\once \\override Fingering  #'extra-offset = #'(")
;; (define postfix ")")
;; (ChangeOffset "something first hello 12.6 . 13.8 etc and something after"  "hello " " etc" (cons "14.2" "55.5") )
(define (ChangeOffset oldstr prefixstring postfixstring offset)
  (let ((startbit "")
	(endbit "")
	(theregexp "")
	(thematch "")
	(oldx "")
	(oldy "")
     
	(xold 0)
	(yold 0)
	(xnew "")
	(ynew "")
	(xval 0)
	(yval 0)
	(xy (string-append " " (car offset) " . " (cdr offset) " ")))
	;(disp "Change offset")
    (begin
      (if (boolean? oldstr)
	  (set! oldstr (string-append prefixstring " 0.0 . 0.0 " postfixstring)))
      (set! startbit (regexp-quote prefixstring))
      (set! endbit  (regexp-quote postfixstring))
      (set! theregexp (string-append startbit "[ ]*([-0-9.]+)[ ]+.[ ]+([-0-9.]+)[ ]*" endbit))
      (set! thematch (string-match theregexp oldstr))
      (if (boolean? thematch)
	  (begin
	    (string-append oldstr prefixstring xy postfixstring))
	  (begin
;;;get the old x y values out of oldstr
      (set! oldx (match:substring thematch 1))
      (set! oldy (match:substring thematch 2))

      (set! xold (string->number oldx))
      (set! yold (string->number oldy))
;;;add two values
      ;;;(set! offset (d-GetOffset))
      ;(disp "Starting with " offset " which is a pair " (pair? offset) " ok?")
      (if (pair? offset)
				(begin
					(set! xnew (car offset))
					(set! ynew (cdr offset))
					(set! xval (string->number xnew))
					(set! yval (string->number ynew))
					(set! xnew (number->string (+ xval xold)))
					(set! ynew (number->string (+ yval yold)))
					(set! xy (string-append xnew " . " ynew)))
				(set! xy " 0.0 . 0.0 "))
				;(disp "the new offset will be " xy " ok?")
	  (regexp-substitute #f thematch 'pre (string-append prefixstring xy postfixstring) 'post))    
    ))));;;; end of function change offset
	
;;;; TweakOffset
;;;Changes the offset of the something at the cursor - at the moment assume standalone or rest
(define (TweakOffset grob tag offsetx offsety)
	(define sa-tag (d-DirectiveGetForTag-standalone ""))
	(if sa-tag		
		(let ((grob (d-DirectiveGet-standalone-grob sa-tag))) ;;; FIXME can we just use the passed in grob
				(if grob
					(cond ((or (equal? grob "RehearsalMark") (equal? grob "BreathingSign")   (equal? grob "MetronomeMark")    )
							(ExtraOffset sa-tag sa-tag "standalone" "Score." (cons offsetx offsety)))
						(#t
							(TweakRelativeOffset sa-tag offsetx offsety)))
					(TweakRelativeOffset sa-tag offsetx offsety)))
		;;; not a standalone directive				
		(begin
			(if tag
				(eval-string (string-append "(d-" tag "  (list (cons 'offsetx \"" offsetx "\")  (cons 'offsety \"" offsety "\")))")))
			(if (Rest?)
				(ExtraOffset "RestOffset" "Rest" "chord" "Voice." (cons offsetx offsety) DENEMO_OVERRIDE_AFFIX)
				(disp "Doing Nothing"))))
	(d-SetSaved #f))
;;;;;;;;;;;
(define (GetSlurPositions)
(let ((yvals #f))
			(set! yvals (d-GetPositions #t))
			(if yvals
				(SetSlurPositions (number->string (car yvals)) (number->string (cdr yvals))))))
				
(define (GetBeamPositions)
		(let ((yvals #f))
			(set! yvals (d-GetPositions #f))
			(if yvals
				(SetBeamPositions (number->string (car yvals)) (number->string (cdr yvals))))))

(define (GetSlurStart)
 	(d-InfoDialog (_"First click on the notehead of the note where the slur starts"))
	(if (d-GetNewTarget) 
		(if (d-IsSlurStart)
			(begin 
				(d-InfoDialog "") 
				(GetSlurPositions))
			(d-InfoDialog (_ "Not a slur start - cancelled")))
		(d-InfoDialog (_ "Cancelled"))))

(define (EditTarget)
	(let ((target (d-GetTargetInfo)) (target-type #f)(grob #f)(tag #f))	
	(define ta-tag "TextAnnotation")
	(define (do-offset)
		(let ((offset #f))
					(set! offset (d-GetOffset))
					(if offset
						(begin
							(TweakOffset grob tag (number->string (car offset)) (number->string (cdr offset)))))))
							
							
(define (do-center-relative-offset)
	(let ((offset #f))
				(d-InfoDialog (_"First click on the center line of the staff\n(Positioning will be done with respect to this height)"))
				(d-GetNewTarget)
				(d-InfoDialog (_"Now click on the position desired for the object"))
				(set! offset (d-GetOffset))
				(if offset
						(begin
							(d-InfoDialog (_ "Re-positioned"))
							(TweakOffset grob tag (number->string (car offset)) (number->string (cdr offset)))))))
							
(define (do-direction)
	(let ((direction #f)
				(choice #f)
				(menu (list (cons (_ "Up")  "^")  (cons (_ "Down")  "_") (cons (_ "Auto")  "-") )) )
						 (set! choice (d-PopupMenu menu))
						  (if choice
								(eval-string (string-append "(d-" tag " (list (cons 'direction \"" choice "\")))")))))
	
							
							
	(define (alter-text)
				(d-TextAnnotation 'edit))
	(define (alter-font-size)
		(define size (d-GetUserInput (_ "Font Size") (_ "Give relative font size: ") "8.0"))
			(if size
					(d-TextAnnotation (cons 'fontsize size))))
				
	(define (chop-beam0)
				(ChopBeaming 0))
	(define (chop-beam1)
				(ChopBeaming 1))
					
				
	;;; the procedure starts here			
	(if target
		(let ((choice #f))
			(set! target-type (list-ref target 0))
			(set! grob (list-ref target 1))
			(set! tag (list-ref target 2))
			(disp "Looking at target " target-type " on grob " grob "with tag " tag " ok?")
			(cond 
				((equal? target-type "Object")
					(if (d-Directive-standalone?)
						(let ((menu ""))
						 (set! menu (list (cons "Offset Position" do-offset)))
						 (if (equal? ta-tag  (GetNthLine (d-DirectiveGetTag-standalone) 0))
								(begin
									(set! menu (cons  (cons "Set Font Size"  alter-font-size)  menu))
									(set! menu (cons  (cons "Alter Text"  alter-text)  menu))))
						 
						 
						 
						 (set! choice (d-PopupMenu menu))
						  (if choice
								(choice)
								(disp "cancelled")))))
								
					((equal? target-type "Chord")
							(let ((menu ""))
								(set! menu (list  (cons (_ "Up/Down") do-direction) (cons (_ "Offset Position") do-center-relative-offset)))   
								;;; FIXME the value is relative to the centre line of the staff, this gets relative to the tr sign.
								;;;need to use d-GetNewTarget to find the notehead position, then use its mid_c_offset to get the centre line value
								;;; beaming does this
								
								
								(set! choice (d-PopupMenu menu))
								(if choice
										(choice)
										(disp "cancelled"))))
								
								
								
				((equal? target-type "Note")
					(if grob   ;;; is grob defined for Fingering, or should this be tag? FIXME
						(cond ((equal? grob "Fingering")
							(set! choice (d-PopupMenu (list (cons (cons "Control Fingerings Positions" 
									"Creates a directive before this chord which can be edited to position the finger indications for each note in the chord") 
										d-FingeringPosition))))
							(if choice
								(choice)
								(disp "cancelled"))))
							(let ((menu '()) (base-duration (d-GetNoteBaseDuration)))
									(set! menu (cons (cons (cons (_ "Line Break") (_ "Start a new line here"))	d-LineBreak) menu))
									(set! menu (cons (cons (cons (_"Page Break") (_"Start a new page here"))	d-PageBreak) menu))
									(if (> base-duration 5)
										(begin
											(set! menu (cons (cons (cons (_"Three Beams Right (Off/On)") (_"Put just three beams to the right or undo a previous invocation of this command")) d-BeamRightThree) menu))
											(set! menu (cons (cons (cons (_"Three Beams Left (Off/On)") (_"Put just three beams to the left or undo a previous invocation of this command")) d-BeamLeftThree) menu))))										
									(if (> base-duration 4)
										(begin
											(set! menu (cons (cons (cons (_"Two Beams Right (Off/On)") (_"Put just two beams to the right or undo a previous invocation of this command")) d-BeamRightTwo) menu))
											(set! menu (cons (cons (cons (_"Two Beams Left (Off/On)") (_"Put just two beams to the left or undo a previous invocation of this command")) d-BeamLeftTwo) menu))					
										))
																					
									(if (> base-duration 3)
										(begin
											(set! menu (cons (cons (cons (_"One Beam Right (Off/On)") (_"Put just one beam to the right or undo a previous invocation of this command")) d-BeamRightOne) menu))
											(set! menu (cons (cons (cons (_"One Beam Left (Off/On)") (_"Put just one beam to the left or undo a previous invocation of this command")) d-BeamLeftOne) menu))
											(set! menu (cons (cons (cons (_"Chop to One Beam") (_"Reduce the beaming between this and the next note to just one beam")) chop-beam1) menu))
											
											))
									(if (> base-duration 2)
									  (begin 
									   (set! menu (cons (cons (cons (_"Chop Gap in Beam") (_"Remove the beaming between this and the next note")) chop-beam0) menu))
									   (set! menu (cons (cons (cons (_"No Beam (Off/On)") (_"Leave note/chord un-beamed or undo a previous invocation of this command")) d-NoBeam) menu))
										 (set! menu (cons (cons (cons (_"Change beam angle/position") (_"Allows you to drag the ends of the beam")) GetBeamPositions) menu))))
			
									(if (d-IsSlurStart)
										(set! menu (cons (cons (cons (_"Hint Slur Angle/Position") (_"Allows you to drag the ends of the slur")) GetSlurPositions) menu )))
									
										
										
										
										
									(set! choice (d-PopupMenu menu))
									(if choice
										(choice)
										(disp "cancelled"))))))))))  ;EditTarget end		
											
;;;; Toggles a postfix annotation on a chord, with editing for direction or offset
(define (ChordAnnotation tag lilypond params graphic)
	(define (set-option option)
					(case (car option)
							((direction) (cdr option))
							((offsetx) (string-append " ^\\tweak #'X-offset #'" (cdr option)))
							((offsety)  (string-append " ^\\tweak #'Y-offset #'" (cdr option)))))
	(define (direction-edit)
		(let ((choice #f))
		(set! choice (d-GetOption  (string-append (_ "Up") stop (_ "Down") stop (_ "Auto") stop)))
		(if choice
				(begin
					(set! choice (cond 	((equal? choice (_ "Up")) "^")
															((equal? choice (_ "Down")) "_")
															((equal? choice (_ "Auto")) "-")))
					(d-DirectivePut-chord-postfix tag (string-append  (string-append choice " " lilypond " ")))
					(d-SetSaved #f)))))
					
	(if (and (d-Directive-chord? tag) (equal? params "edit"))
						(case (GetEditOption)
								((edit) (direction-edit))
								((cancel) #f)
								((advanced) (d-DirectiveTextEdit-chord tag))
								((delete) (d-DirectiveDelete-chord tag))
								(else #f))
						(if params
							(begin 
								(if (and (d-Directive-chord? tag) (list? params))
									(begin
										(d-SetSaved #f)
										(d-DirectivePut-chord-postfix tag (string-append  (string-join (map set-option params)) " " lilypond " ")))
									(d-WarningDialog "Cannot complete operation - cursor moved or bad parameter list")))
							(begin  ;;;no parameters, toggle annotation off/on
											
											(ToggleChordDirective tag graphic lilypond DENEMO_OVERRIDE_ABOVE)))))
											
											
																
; SetSlurPositions
(define (SetSlurPositions near far)
	(d-DirectivePut-chord-override "Slur" DENEMO_OVERRIDE_AFFIX)
  (d-DirectivePut-chord-prefix "Slur" (string-append 
  	"\\once \\override Slur #'direction = #" (if (or (> (string->number near) 0) (> (string->number far) 0)) "1" "-1") " "
  "\\once \\override Slur  #'positions = #'(" near " . " far ")"))
  (d-SetSaved #f))
  
  
;;currently SetBeamPositions is just testing for offset with respect to 0, in fact it is the offset from the center staff line
;;that should be used. This would require determining which clef is in use and what the notes at start and end of the beamed notes are  
; SetBeamPositions
(define (SetBeamPositions near far)
	(d-DirectivePut-chord-override "Beam" DENEMO_OVERRIDE_AFFIX)
  (d-DirectivePut-chord-prefix "Beam" (string-append 
		 (if (or (> (string->number near) 0) (> (string->number far) 0)) "\\stemUp" "\\stemDown") " "
			"\\once \\override Beam  #'positions = #'(" near " . " far ")"))
  (d-SetSaved #f))

  
;;;;;;;; ChangePad
(define (ChangePad oldstr prefixstring postfixstring)
  (ChangeValue oldstr prefixstring postfixstring d-GetPadding "0"))
;;;;;;;; ChangeRelativeFontSize
(define (ChangeRelativeFontSize oldstr prefixstring postfixstring)
  (ChangeValue oldstr prefixstring postfixstring d-GetRelativeFontSize "0" DENEMO_OVERRIDE_AFFIX))


;   (let ((startbit "")
; 	(endbit "")
; 	(theregexp "")
; 	(thematch "")
; 	(pad "")
; 	)
;     (begin
;       (if (boolean? oldstr)
; 	  (set! oldstr (string-append prefixstring "0" postfixstring)))
;       (set! startbit (regexp-quote prefixstring))
;       (set! endbit  (regexp-quote postfixstring))
;       (set! theregexp (string-append  startbit "([-0-9]+)" endbit))
;       (set! thematch (string-match theregexp oldstr))
;       (set! pad (d-GetPadding))
;       (if (boolean? pad)
; 	  (set! pad "0"))
;       (if (boolean? thematch)
; 	  (begin
; 	    (string-append oldstr prefixstring pad postfixstring))
; 	  (regexp-substitute #f thematch 'pre (string-append prefixstring pad postfixstring) 'post))    
;     )));;;; end of function change pad

;;;;;;;; ChangeValue
(define (ChangeValue oldstr prefixstring postfixstring val)
  (let ((startbit "")
	(endbit "")
	(theregexp "")
	(thematch "")
	)
    (begin
      (set! startbit (regexp-quote prefixstring))
      (set! endbit  (regexp-quote postfixstring))
      (set! theregexp (string-append  startbit "([-0-9]+)" endbit))
      (set! thematch (string-match theregexp oldstr))
      
      (if (boolean? thematch)
	  (begin
	    (string-append oldstr prefixstring val postfixstring))
	  (regexp-substitute #f thematch 'pre (string-append prefixstring val postfixstring) 'post))    
    )));;;; end of function change value

