;;; routines for manipulating the evince widget's display for wysiwyg operations on the print view.

;;returns the nth line counting from 0, returns "" if not enough lines in text.
(define (GetNthLine text n)
	(let ((thelist (string-split text #\newline)))
		(if (> (length thelist) n)
		(list-ref thelist n)
		"")))



;; current values of offset are stored in display field on second and third lines
; new values are relative to those
(define (TweakRelativeOffset tag offsetx offsety)
	(define text (d-DirectiveGet-standalone-display tag))

	(define curx #f)
	(define cury #f)
	(set! curx (string->number (GetNthLine text 1)))
	(set! cury (string->number (GetNthLine text 2)))
	(if curx
		(begin
			(set! offsetx (number->string (+ (string->number offsetx) curx)))
			(set! offsety (number->string (+ (string->number offsety) cury)))))
	(d-DirectivePut-standalone-display tag (string-append (GetNthLine text 0) "\n" offsetx "\n" offsety))
	(d-DirectivePut-standalone-prefix tag (string-append "<>-\\tweak #'extra-offset #'(" offsetx " . " offsety ") -")))
	
;;;; TweakOffset
;;;Changes the offset of the something at the cursor - at the moment assume standalone or rest
(define (TweakOffset offsetx offsety)
	(define tag (d-DirectiveGetForTag-standalone ""))
	(if tag		
			(let ((grob (d-DirectiveGet-standalone-grob tag)))
				(if grob
					(cond ((or (equal? grob "RehearsalMark") (equal? grob "BreathingSign"))
							(ExtraOffset tag "standalone" "Score." (cons offsetx offsety)))
						(#t
							(TweakRelativeOffset tag offsetx offsety)))
					(TweakRelativeOffset tag offsetx offsety))
			
			)
		;;; not a standalone directive				
		(begin
			(if (Rest?)
				(ExtraOffset "Rest" "chord" "Voice." (cons offsetx offsety) DENEMO_OVERRIDE_AFFIX)
				(disp "Doing Nothing") ;;(AlterPositions "Slur" "chord" "" (cons offsetx offsety) DENEMO_OVERRIDE_AFFIX)	
				)))
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
	(let ((target (d-GetTargetInfo)) (target-type #f)(grob #f))	
	(define ta-tag "TextAnnotation")
	(define (do-offset)
		(let ((offset #f))
					(set! offset (d-GetOffset))
					(if offset
						(begin
							(TweakOffset (number->string (car offset)) (number->string (cdr offset)))))))
			(define (alter-text)
				(d-TextAnnotation 'edit))
;;; the procedure starts here			
	(if target
		(let ((choice #f))
			(set! target-type (list-ref target 0))
			(set! grob (list-ref target 1))
			(disp "Looking at target " target-type " on grob " grob " ok?")
			(cond 
				((equal? target-type "Object")
					(if (d-Directive-standalone?)
						(let ((menu ""))
						 (set! menu (list (cons "Offset Position" do-offset)))
						 (if (equal? ta-tag  (GetNthLine (d-DirectiveGetTag-standalone) 0))
								(begin
									(set! menu (cons  (cons "Alter Text"  alter-text)  menu))))
						 
						 
						 
						 (set! choice (d-PopupMenu menu))
						  (if choice
								(choice)
								(disp "cancelled")))))
								
					((equal? target-type "Chord")			
							(let ((menu ""))
								(set! menu (list (cons "Offset Position" do-offset)))
								(set! choice (d-PopupMenu menu))
								(if choice
										(choice)
										(disp "cancelled"))))
								
								
								
				((equal? target-type "Note")
					(if grob
						(cond ((equal? grob "Fingering")
							(set! choice (d-PopupMenu (list (cons (cons "Control Fingerings Positions" 
									"Creates a directive before this chord which can be edited to position the finger indications for each note in the chord") 
										d-FingeringPosition))))
							(if choice
								(choice)
								(disp "cancelled"))		

										))
						(let ((menu '()))
									(set! menu (cons (cons (cons (_ "Line Break") (_ "Start a new line here"))	d-LineBreak) menu))
									(set! menu (cons (cons (cons (_"Page Break") (_"Start a new page here"))	d-PageBreak) menu))
										
									(if (d-IsSlurStart)
										(set! menu (cons (cons (cons (_"Hint Slur Angle/Position") (_"Allows you to drag the ends of the slur")) GetSlurPositions) menu )))
									(if (> (d-GetNoteBaseDuration) 2)
										(set! menu (cons (cons (cons (_"Change beam angle/position") (_"Allows you to drag the ends of the beam")) GetBeamPositions) menu)))
									(set! choice (d-PopupMenu menu))
									(if choice
										(choice)
										(disp "cancelled"))))))))))		
										
						
										
										
										;)))))))
			
			
					
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
(define (ChangeValue oldstr prefixstring postfixstring get-func default-val)
  (let ((startbit "")
	(endbit "")
	(theregexp "")
	(thematch "")
	(pad "")
	)
    (begin
      (if (boolean? oldstr)
	  (set! oldstr (string-append prefixstring default-val postfixstring)))
      (set! startbit (regexp-quote prefixstring))
      (set! endbit  (regexp-quote postfixstring))
      (set! theregexp (string-append  startbit "([-0-9]+)" endbit))
      (set! thematch (string-match theregexp oldstr))
      (set! pad (get-func))
      (if (boolean? pad)
	  (set! pad default-val))
      (if (boolean? thematch)
	  (begin
	    (string-append oldstr prefixstring pad postfixstring))
	  (regexp-substitute #f thematch 'pre (string-append prefixstring pad postfixstring) 'post))    
    )));;;; end of function change pad
