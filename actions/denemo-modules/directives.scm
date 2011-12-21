; Prototype to insert Lilypond Standalone Directives. Wants a pair with car Tag and cdr lilypond: (cons "BreathMark" "\\breathe")
(define* (StandAloneDirectiveProto pair #:optional (step? #t) (graphic #f) (displaytext #f))
	(d-Directive-standalone (car pair))
	(d-DirectivePut-standalone-postfix (car pair) (cdr pair))
	(d-DirectivePut-standalone-minpixels (car pair) 30)
	(if graphic ;If the user specified a graphic use this, else greate a display text
		(begin (d-DirectivePut-standalone-graphic (car pair) graphic)
			   (d-DirectivePut-standalone-override (car pair) DENEMO_OVERRIDE_GRAPHIC))
		(if displaytext
		  (d-DirectivePut-standalone-display (car pair) displaytext)
		  (d-DirectivePut-standalone-display (car pair) (cdr pair)))
		)
	(if step?
		(d-MoveCursorRight))
	(d-RefreshDisplay))


; Procedure to insert Self-Editing Lilypond Standalone Directives. Takes a pair with car Tag and cdr lilypond: (cons "BreathMark" "\\breathe") with optional boolean to step right after insertion and graphic
(define* (StandAloneSelfEditDirective pair #:optional (step? #t) (graphic #f))
	(if (d-Directive-standalone? (car pair))
	  (d-DirectiveTextEdit-standalone (car pair))
	  (StandAloneDirectiveProto pair step? graphic)))

;Wrapper to attach any lilypond directive anywhere.
;;Wants four strings and an arbitrary number of tags (numbers) for overrides.
;;the tag parameter can be a single string or a pair. A single string is both the tag and display, a pair is (cons "tag" "display") 
(define (AttachDirective type field tag content . overrides)
	(define proc-put (string-append "d-DirectivePut-" type "-" field))
	;(define proc-get (string-append "d-DirectiveGet-" type "-" field))
	;(define proc-del (string-append "d-DirectiveDelete-" type))
	(define proc-dis (string-append "d-DirectivePut-" type "-display"))
	(define proc-ovr (string-append "d-DirectivePut-" type "-override"))
	(define dis #f)
	(if (pair? tag)
		(begin (set! dis (cdr tag)) (set! tag (car tag)))
		(set! dis tag))
	(d-SetSaved #f)
	((eval-string proc-put) tag content)
	(if (member DENEMO_OVERRIDE_GRAPHIC overrides) ; If DENEMO_OVERRIDE_GRAPHIC is there just go on
		((eval-string proc-ovr) tag (apply logior overrides))
		(if (equal? type "staff") ; if not test if its a staff directive: we must enforce graphic to make sure staff-icons work.
			((eval-string proc-ovr) tag (apply logior (append (list DENEMO_OVERRIDE_GRAPHIC) overrides)))
			((eval-string proc-ovr) tag (apply logior overrides)))) ; not a staff, everythings ok without DENEMO_OVERRIDE_GRAPHIC
	((eval-string proc-dis) tag dis)
	#t)
	
; ToggleDirective is a script to help you by creating and deleting Denemo-Directives with the same command.
;; return value is #t if directive was created or #f if it was deleted. This can be used as hook for further scripting.
;; example (ToggleDirective "staff" "prefix" "Ambitus" "\\with { \\consists \"Ambitus_engraver\" }")
(define (ToggleDirective type field tag content . overrides) ; four strings and an arbitrary number of tags (numbers) for overrides.
	(define proc-get (string-append "d-DirectiveGet-" type "-" field))
	(define proc-del (string-append "d-DirectiveDelete-" type))
	(define dis #f)
	(if (pair? tag)
		(begin (set! dis (cdr tag)) (set! tag (car tag)))
		(set! dis tag))		
	(if ((eval-string proc-get) tag)
		(begin	((eval-string proc-del) tag)
				(d-SetSaved #f)
				#f)
	(apply AttachDirective type field (cons tag dis) content overrides)))
	  

; d-DirectivePut-standalone a convenience function for standalone directives
(define (d-DirectivePut-standalone tag)
  (d-DirectivePut-standalone-minpixels tag 0)
  (d-MoveCursorLeft))

(define (d-Directive-standalone tag)
  (if (not (d-Directive-standalone? tag))
      (d-DirectivePut-standalone tag)))

(define* (d-Directive-standalone?  #:optional (tag #f))
  (if (equal? tag #f)
      (string? (d-DirectiveGetForTag-standalone ""))
      (equal? tag (d-DirectiveGetForTag-standalone tag))))
(define (d-DirectiveGetTag-standalone)
  (d-DirectiveGetForTag-standalone ""))

(define* (d-Directive-chord?  #:optional (tag #f))
  (if (equal? tag #f)
      (string? (d-DirectiveGetForTag-chord ""))
      (equal? tag (d-DirectiveGetForTag-chord tag))))
(define (d-DirectiveGetTag-chord)
  (d-DirectiveGetForTag-chord ""))

(define* (d-Directive-note?  #:optional (tag #f))
  (if (equal? tag #f)
      (string? (d-DirectiveGetForTag-note ""))
      (equal? tag (d-DirectiveGetForTag-note tag))))
(define (d-DirectiveGetTag-note)
  (d-DirectiveGetForTag-note ""))

(define* (d-Directive-score?  #:optional (tag #f))
  (if (equal? tag #f)
      (string? (d-DirectiveGetForTag-score ""))
      (equal? tag (d-DirectiveGetForTag-score tag))))
(define (d-DirectiveGetTag-score)
  (d-DirectiveGetForTag-score ""))

(define* (d-Directive-scoreheader?  #:optional (tag #f))
  (if (equal? tag #f)
      (string? (d-DirectiveGetForTag-scoreheader ""))
      (equal? tag (d-DirectiveGetForTag-scoreheader tag))))
(define (d-DirectiveGetTag-scoreheader)
  (d-DirectiveGetForTag-scoreheader ""))


(define* (d-Directive-movementcontrol?  #:optional (tag #f))
  (if (equal? tag #f)
      (string? (d-DirectiveGetForTag-movementcontrol ""))
      (equal? tag (d-DirectiveGetForTag-movementcontrol tag))))
(define (d-DirectiveGetTag-movementcontrol)
  (d-DirectiveGetForTag-movementcontrol ""))

(define* (d-Directive-header?  #:optional (tag #f))
  (if (equal? tag #f)
      (string? (d-DirectiveGetForTag-header ""))
      (equal? tag (d-DirectiveGetForTag-header tag))))
(define (d-DirectiveGetTag-header)
  (d-DirectiveGetForTag-header ""))


(define* (d-Directive-paper?  #:optional (tag #f))
  (if (equal? tag #f)
      (string? (d-DirectiveGetForTag-paper ""))
      (equal? tag (d-DirectiveGetForTag-paper tag))))
(define (d-DirectiveGetTag-paper)
  (d-DirectiveGetForTag-paper ""))


(define* (d-Directive-layout?  #:optional (tag #f))
  (if (equal? tag #f)
      (string? (d-DirectiveGetForTag-layout ""))
      (equal? tag (d-DirectiveGetForTag-layout tag))))
(define (d-DirectiveGetTag-layout)
  (d-DirectiveGetForTag-layout ""))


(define* (d-Directive-staff?  #:optional (tag #f))
  (if (equal? tag #f)
      (string? (d-DirectiveGetForTag-staff ""))
      (equal? tag (d-DirectiveGetForTag-staff tag))))
(define (d-DirectiveGetTag-staff)
  (d-DirectiveGetForTag-staff ""))


(define* (d-Directive-voice?  #:optional (tag #f))
  (if (equal? tag #f)
      (string? (d-DirectiveGetForTag-voice ""))
      (equal? tag (d-DirectiveGetForTag-voice tag))))
(define (d-DirectiveGetTag-voice)
  (d-DirectiveGetForTag-voice ""))


(define* (d-Directive-clef?  #:optional (tag #f))
  (if (equal? tag #f)
      (string? (d-DirectiveGetForTag-clef ""))
      (equal? tag (d-DirectiveGetForTag-clef tag))))
(define (d-DirectiveGetTag-clef)
  (d-DirectiveGetForTag-clef ""))


(define* (d-Directive-keysig?  #:optional (tag #f))
  (if (equal? tag #f)
      (string? (d-DirectiveGetForTag-keysig ""))
      (equal? tag (d-DirectiveGetForTag-keysig tag))))
(define (d-DirectiveGetTag-keysig)
  (d-DirectiveGetForTag-keysig ""))


(define* (d-Directive-timesig?  #:optional (tag #f))
  (if (equal? tag #f)
      (string? (d-DirectiveGetForTag-timesig ""))
      (equal? tag (d-DirectiveGetForTag-timesig tag))))
(define (d-DirectiveGetTag-timesig)
  (d-DirectiveGetForTag-timesig ""))
  
  
(define (d-DirectiveDelete-standalone Tag)
	(if (equal? (d-DirectiveGetTag-standalone) Tag)
	(begin (d-DeleteObject) #t)
	#f))

(define* (ToggleChordDirective tag fontname lilypond #:optional (override #f))
	(if (d-Directive-chord? tag)
          (d-DirectiveDelete-chord tag)
          (begin
            (d-DirectivePut-chord-graphic tag fontname)
            (d-DirectivePut-chord-postfix tag lilypond)
            (if (equal? override "alt")
	      (d-DirectivePut-chord-override tag DENEMO_ALT_OVERRIDE))
	    (if (equal? override "above")
              (d-DirectivePut-chord-override tag DENEMO_OVERRIDE_ABOVE)))))
