(use-modules (srfi srfi-13))
(use-modules (ice-9 regex))
(use-modules (ice-9 optargs))
(load "ans-7.scm")

(define DenemoKeypressActivatedCommand #f);;;is true while a keyboard shortcut is invoking a script, unless the script has set it to #f

;;;;;;;;;; create documentation for a command - this version just prints out basic info
;;;;;;;;;;;;;DocumentCommand
(define (DocumentCommand name)
  (let ((help (d-GetHelp name)))
    (if (boolean? help)
	(begin
	  (set! help (string-append "Help-d-" name))	  
	  (let ((sym (with-input-from-string help read)))
	    (if (defined? sym)
		(set! help (eval sym (current-module)))
		(set! help "No help")
		))))
    (format #t "~%~%Command ~A~%Tooltip ~A~%Label ~A~%Menu Path ~A~%" name help (d-GetLabel name) (d-GetMenuPath name))))
;;;;;;;;;;;;;;; 

;;;;;;;;;;;;Replace a part of a string

(define (replace-nth list n elem)
  (cond 
    ((null? list) ())
    ((eq? n 0) (cons elem (cdr list)))
    (#t (cons(car list) (replace-nth (cdr list) (- n 1) elem)))))


;;;;;;;;;;;;;; Get highest and lowest note as lilypond syntax. Works on Chords and Single Notes.
;;;;;;;;;;;;;; GetNotes returns a string of lily-notes from low to high. Make a list out of them and refer to the first (0) element or last (length -1) one.
;;;;;;;;;;;;;; Returns #f if not a chord

(define (d-GetLowestNote)
(if (d-GetNotes)
 (list-ref (string-tokenize(d-GetNotes)) 0 )
 #f
 ))

(define (d-GetHighestNote)
 (if (d-GetNotes)
 (list-ref (string-tokenize(d-GetNotes)) (- (length (string-tokenize(d-GetNotes)) ) 1 ))
 #f
 ))


(define (d-CursorGoUp x x-max dx) ;Cursor goes up a specific amount
   (if (<= x x-max)
      (begin
      	(d-CursorUp)
        (d-CursorGoUp (+ x dx) x-max dx))))

(define (d-CursorGoDown x x-max dx)  ;Cursor goes down a specific amount
   (if (<= x x-max)
      (begin
         (d-CursorDown)
         (d-CursorGoDown (+ x dx) x-max dx))))

;Next Selected Object for all Staffs by Nils Gey Feb/2010
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Moves the cursor to the right. If there is no selection, If that returns #f it will move down one staff and rewind to the start of the selection in this staff..
;FIXME: After the last selected Item the cursor will move down one staff even outside the selection and will stay there. But a script with SingleAndSelectionSwitcher will NOT be applied to this outside-object and the user will not see this because the cursor is returned to the starting position afterwards.

(define (NextSelectedObjectAllStaffs)
	(if  (not (d-NextSelectedObject)) 
		(if  (and (d-MoveToStaffDown) (d-NextSelectedObject) (d-PrevSelectedObject))
		(begin 
		 (let loop ()
			(if (d-PrevSelectedObject)
				(loop)))
		#t); block end. 
		#f ; if !StaffDown
		); fi StaffDown
	#t ;NextSelecetedObject was succesful
	)
)


;SingleAndSelectionSwitcher by Nils Gey Jan/2010
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Automatically applies a script to a whole selection. You can give different commands or command blocks with (begin) for single items or whole selections. You can enter a complete scheme script with (),  arguments and everything you would want to run standalone. Don't forget to escape chars like  \" . You can even use a complete (begin ) block.
;But attention! SingleAndSelectionSwitcher will still try to apply the given script to each of the single items alone. If you need a script which differs completly in beaviour for single/selection you have to write your own. You have to take out the (let loop () section for this and write your own selection part there.
;The applied script itself has to take care if the command can be applied to each potential item. If you want only notes/chords/rests you have to make sure the script does not abort on other objects. Its the same as giving proper return values for a single item, just return #f if a command is not possible for an item. While a single item just returns an error if you don't do it correctly, but does no harm otherwise, a script applied to a selection will stop on that item and leaves you on half on the way.
;Return values are the return values the script itself gives.
;Example: (SingleAndSelectionSwitcher  "(d-ChangeDurationByFactorTwo *)" "(d-ChangeDurationByFactorTwo *)")

(define (SingleAndSelectionSwitcher commandsingle commandselection)
(d-PushPosition)
(if (and DenemoPref_applytoselection (d-GoToSelectionStart))
(begin
	(eval-string  commandselection)
	(let loop ()
	(if (NextSelectedObjectAllStaffs)
	 	(begin (eval-string  commandselection) (loop))
	))
	(d-GoToSelectionStart)
	(d-PopPosition)
	)
(begin	
	(eval-string commandsingle)	)
))
;; End of SingleAndSelectionSwitcher

(define Chord? (lambda ()
		  (string=? (d-GetType) "CHORD")))

(define NextChordInSelection (lambda () (if (d-NextSelectedObject) 
					    (if (Chord?)
			                	 #t
			                	 (NextChordInSelection))
					    #f
					    )))
(define FirstChordInSelection (lambda () (if (d-GoToMark)
						  (if (Chord?)
			                	 #t)
						  #f)))
				    
(define ApplyToSelection (lambda (command positioning_command)
			   (begin
			     (if (eval-string positioning_command)
				 (begin
				    (eval-string  command)
				    (ApplyToSelection command "(d-NextSelectedObject)"))))))
(define (PrevDirectiveOfTag tag)
  (let loop ()
    (if (d-PrevStandaloneDirective)
       (if (not (d-Directive-standalone? tag))
	   (loop)
	   #t
	   )
       #f)))
(define (NextDirectiveOfTag tag)
  (let loop ()
    (if (d-NextStandaloneDirective)
       (if (not (d-Directive-standalone? tag))
	   (loop)
	   #t
	   )
       #f)))


(define stop "\0")
(define cue-Advanced "Advanced")
(define cue-PlaceAbove "Place above staff")
(define cue-PlaceBelow "Place below staff")
(define cue-SetRelativeFontSize "Set Relative Font Size")
(define cue-OffsetPositionAll "Offset Position (All)")
(define cue-OffsetPositionOne "Offset Position (One)")
(define cue-EditText "Edit Text")
(define cue-SetPadding "Set Padding")
(define cue-Delete "Delete")
;(define cue- "")


;;;;;;;;;;;;;;;; Double-Stroke for sequencing keypresses. By Nils Gey June 2010
;One parameter for the GUI-version or help window. This is the version that appears if someone clicks on the menu version.
;Ten optional parameters given as strings which can be any scheme command, but in "" and with escaped \" in them. They return #f if not defined
;gui-version can be any command to aid the user. Most likely it will we a tooltip or better a GUI with radio buttons with all commands (if (not #f) ...) and help texts and maybe additional parameters.
;Right now its hardwired to the number keys and space for help. The reason is because the keybindings for number keys can change. If there were wrapped-commands for numbers (which they were some time ago) this script could be done better with (d-GetCommand) instead of (d-GetKeypress). Its also possible to create an even more insane version with 20 optional parameters, 10 for the actions, 10 for the keys.

(define* (doublestroke gui-version #:optional (first "#f") (second "#f") (third "#f") (fourth "#f") (fifth "#f") (sixth "#f") (seventh "#f") (eighth "#f") (nineth "#f") (tenth "#f"))
(if DenemoKeypressActivatedCommand
	(begin (case (string->symbol (d-GetKeypress))
		((#{1}#)  (eval-string first))
		((#{2}#)  (eval-string second))
		((#{3}#)  (eval-string third))
		((#{4}#)  (eval-string fourth))
		((#{5}#)  (eval-string fifth))
		((#{6}#)  (eval-string sixth))
		((#{7}#)  (eval-string seventh))
		((#{8}#)  (eval-string eighth))
		((#{9}#)  (eval-string nineth))
		((#{0}#)  (eval-string tenth))
		((space)  (eval-string gui-version))
		(else #f))
	  (set! DenemoKeypressActivatedCommand #f))
	  
	  (eval-string gui-version)) ; else
	 
)

	;Example:
	; (doublestroke "(d-WarningDialog \"After invoking the command, what you already have done right now, press a number key to specify number to print to the console or any other key to abort.\n\")" 
	;  "(display \"1\")"  "(display \"2\")"  "(display \"3\")"  "(display \"4\")"  "(display \"5\")"  "(display \"6\")"  "(display \"7\")"  "(display \"8\")"  "(display \"9\")" "(display \"0\")")





;;;;;;;;;;;;;;;;; ExtraOffset
;;; the parameter "what" is the LilyPond grob that is being tweaked - it may not be the tag of the DenemoDirective that is being edited
(define* (ExtraOffset what  #:optional (type "chord") (context ""))
  (let ((tag "")(oldstr #f) (start "") (end "") (get-command d-DirectiveGet-chord-prefix)  (put-command d-DirectivePut-chord-prefix))
    (cond
     ((string=? type "note")
      (begin (set! get-command d-DirectiveGet-note-prefix)
	     (set! put-command d-DirectivePut-note-prefix)))
     ((string=? type "standalone")
      (begin (set! get-command d-DirectiveGet-standalone-prefix)
	     (set! put-command d-DirectivePut-standalone-prefix)))
     )

    (set! tag what)
    (set! oldstr (get-command tag))
    (if (equal? oldstr "")
	(set! oldstr #f))
(display oldstr)
    (set! start (string-append "\\once \\override " context what " #'extra-offset = #'("))
    (set! end ")")
    (put-command tag (ChangeOffset oldstr start end))))

;;;;;;;;;;;;;;;;; SetRelativeFontSize
(define* (SetRelativeFontSize what #:optional (type "chord") (context ""))
  (SetValue ChangeRelativeFontSize " #'font-size = #" what type context))



;;;;;;;;;;;;;;;;; SetPadding
(define* (SetPadding what  #:optional (type "chord") (context ""))
  (SetValue ChangePad " #'padding = #" what type context))

;;;;;;;;;;;;;;;;; SetValue
(define* (SetValue change-func change-str  what  #:optional (type "chord") (context ""))
  (let ((tag "") (oldstr #f) (start "") (end "") (pad "")  (get-command d-DirectiveGet-chord-prefix) (put-command d-DirectivePut-chord-prefix))
    (cond
     ((string=? type "note")
      (begin (set! get-command d-DirectiveGet-note-prefix)
	     (set! put-command d-DirectivePut-note-prefix)))
     ((string=? type "standalone")
      (begin (set! get-command d-DirectiveGet-standalone-prefix) 
	     (set! put-command d-DirectivePut-standalone-prefix)))
     )
    (set! start (string-append "\\once \\override " context what change-str))
    (set! end " ")
    (set! tag what)
    (set! oldstr (get-command tag))
    (if (equal? oldstr "")
	(set! oldstr #f))
    (put-command tag (change-func oldstr start end))))



;;;;;;;;;;;;;;;;; ChangeOffset
;;; e.g.  (define prefixstring      "\\once \\override Fingering  #'extra-offset = #'(")
;;; (define postfix ")")

(define (ChangeOffset oldstr prefixstring postfixstring)
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
	(xy " 0.0 . 0.0 ")
	(offset ""))
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
      (set! offset (d-GetOffset))
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
	  (regexp-substitute #f thematch 'pre (string-append prefixstring xy postfixstring) 'post))    
    ))));;;; end of function change offset

;;;;;;;; ChangePad
(define (ChangePad oldstr prefixstring postfixstring)
  (ChangeValue oldstr prefixstring postfixstring d-GetPadding "0"))
;;;;;;;; ChangeRelativeFontSize
(define (ChangeRelativeFontSize oldstr prefixstring postfixstring)
  (ChangeValue oldstr prefixstring postfixstring d-GetRelativeFontSize "0"))


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



;;;;;;;;;; SetHeaderField sets a field in the movement header
;;;;;;;;;; the directive created is tagged Score or Movement depending on the field

(define* (SetHeaderField field #:optional (title #f))
  (let ((current "") (thematch #f) (tag "") (type "") (fieldname ""))
    (if 
     (or (equal? field "subtitle") (equal? field "subsubtitle") (equal? field "piece"))
     (begin
       (set! type "Movement")
       (if (equal? field "subtitle")
	   (set! fieldname "Title"))
        (if (equal? field "subsubtitle")
	   (set! fieldname "Subtitle"))
	(if (equal? field "piece")
	   (set! fieldname "Piece")) )
     (begin
       (set! type "Score")
       (set! fieldname (string-capitalize field))))
     
    (set! tag (string-append type fieldname)) 
    (set! current (d-DirectiveGet-header-postfix tag))
    (if (boolean? current)
	(set! current "") 
	(begin
	  ;;(display current)
	  (set! thematch (string-match (string-append field " = \"([^\"]*)\"\n") current))
	  ;;(display thematch)
	  (if (regexp-match? thematch)
	      (set! current (match:substring thematch 1)))))
    (if (boolean? title)
	(set! title (d-GetUserInput (string-append type " " fieldname)
				    (string-append "Give a name for the " fieldname " of the " type) current)))
    (d-DirectivePut-header-override tag DENEMO_OVERRIDE_GRAPHIC)
    (d-DirectivePut-header-display tag (string-append type " " fieldname ": " title))
    
    (d-DirectivePut-header-postfix tag (string-append field " = \"" title "\"\n"))))

;;;;;;;;;; SetScoreHeaderField sets a field in the score header

(define (SetScoreHeaderField field)
(let ((title "") (current "") (thematch #f) (tag ""))
  (set! tag (string-append "Score" (string-capitalize field)))
  (set! current (d-DirectiveGet-scoreheader-postfix tag))
  (if (boolean? current)
      (set! current "") 
      (begin
	;;(display current)
	(set! thematch (string-match (string-append field " = \"([^\"]*)\"\n") current))
	;;(display thematch)
	(if (regexp-match? thematch)
	    (set! current (match:substring thematch 1)))))
  (set! title (d-GetUserInput (string-append "Score " field) 
			      (string-append "Give a name for the " field " of the whole score") current))
  (d-DirectivePut-scoreheader-override tag DENEMO_OVERRIDE_GRAPHIC)
  (d-DirectivePut-scoreheader-display tag (string-append field ": " title))
  (d-DirectivePut-scoreheader-postfix tag (string-append field " = \"" title "\"\n"))))

;;;; d-DirectivePut-standalone a convenience function for standalone directives
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

(define (CreateButton tag label)
  (d-DirectivePut-score-override tag DENEMO_OVERRIDE_GRAPHIC)
  (d-DirectivePut-score-display tag label))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;; String Escaper
;;;;;;;;; Escapes Strings.
;;; from brlewis http://srfi.schemers.org/srfi-13/mail-archive/msg00025.html

(define (string-escaper esc)
  (let ((spec (char-escape-spec esc)))
    (lambda (str) (string-escape str spec))))

(define (string-needs-escape? str esc)
  (let ((len (string-length str)))
    (let loop ((i 0))
      (if (= i len)
	  #f
	  (let ((c (string-ref str i)))
	    (if (and (char>=? c (car esc))
		     (char<=? c (cadr esc)))
		#t
		(loop (+ 1 i))))))))

(define (string-escape str esc)
  (if (string-needs-escape? str esc)
      (list->string
       (reverse
	(let ((len (string-length str)))
	  (let loop ((i 0)
		     (li '()))
	    (if (= i len)
		li
		(loop (+ 1 i)
		      (let ((c (string-ref str i)))
			(if (and (char>=? c (car esc))
				 (char<=? c (cadr esc)))
			    (let ((li2 (vector-ref
					(caddr esc)
					(- (char->integer c)
					   (char->integer (car esc))))))
			      (if li2
				  (append li2 li)
				  (cons c li)))
			    (cons c li)))))))))
      str))

(define (char-escape-spec speclist)
  (let ((minchar (caar speclist))
	(maxchar (caar speclist)))
    (let loop ((li (cdr speclist)))
      (if (not (null? li))
	  (begin
	    (let ((testchar (caar li)))
	      (if (char<? testchar minchar)
		  (set! minchar testchar))
	      (if (char>? testchar maxchar)
		  (set! maxchar testchar)))
	    (loop (cdr li)))))
    (list
     minchar
     maxchar
     (let ((specv (make-vector (+ 1 (- (char->integer maxchar)
				       (char->integer minchar))) #f)))
      (map (lambda (specpair)
	     (vector-set! specv
			  (- (char->integer (car specpair))
			     (char->integer minchar))
			  (reverse (string->list (cdr specpair)))))
	   speclist)
      specv))))
	  
;; examples of use

(define html-escape (string-escaper '((#\< . "&lt;")
				      (#\> . "&gt;")
				      (#\& . "&amp;"))))

(define scheme-escape (string-escaper '((#\\ . "\\\\")
					(#\" . "\\\""))))

(define latex-escape (string-escaper '((#\\ . "\\\\")
				       (#\~ . "\\~")
				       (#\# . "\\#")
				       (#\$ . "\\$")
				       (#\% . "\\%")
				       (#\^ . "\\^")
				       (#\& . "\\&")
				       (#\{ . "\\{")
				       (#\} . "\\}")
				       (#\_ . "\\_"))))

;;;;;;;;;; Parse strings for json values
(define (ParseJson target key)
  (let ((theregexp #f) (thematch #f))

 ;;; this is the regexp to find "key":"something" with the something being the match, ie in ()
(set! theregexp (string-append "\"" key "\":\"([^\"]*)\""))

;;;;; this gets a match structure for target, so if there is a match
(set! thematch (string-match theregexp target))

(if (regexp-match? thematch) ;;; if there was a match return it
   (match:substring thematch 1)
    ;;;if there was no match return #f
    #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; This is Denemos interface to access the MediaWiki API (http://www.mediawiki.org/wiki/API), which is used for the current Denemo-Website
;;;; Send any question to Nils "Steele" Gey list@nilsgey.de
;;;; Currently its only used to create/overwrite a page with a new script.
;;;; It uses the User-Rights System so its very secure. Vandalism in only possible in the same degree as allowed on the website itself.
;;;; All API access is done via (d-HTTP). The C function behind it sends HTTP-POST data to the given Server/Website and returns the HTTP-header and MediaWiki Data. 
;;;;
;;;; The basic steps are 1)Login with Username/PW given in Denemos Preferences and 2)Create a HTTP-Cookie .
;;;; After that allowed Manipulation is possible. Currently we create request an Edit-Token and create a new Page.
;;;; 

(define (d-UploadRoutine list)
  (define command (list-ref list 0))
  (define name (list-ref list 1))
  (define script (list-ref list 2))
  (define initscript (list-ref list 3))
  (define menupath (list-ref list 4))
  (define label (list-ref list 5))
  (define tooltip (list-ref list 6))
  (define after (list-ref list 7))

  ;Some constants. Change these only if the Website moves.
  (define HTTPHostname "www.denemo.org") ; don't use http:// . No tailing /
  (define HTTPSite "/api.php")   
	
	; Prepare Login. Use this only once in (CookieString) because all tokens change on any new login.
	(define (LogMeIn) 
			(d-HTTP ;Parameters are hostname, site, cookies/header and POST-body
			HTTPHostname
			HTTPSite
			"" ; Cookie entrypoint. No Cookie for now.
			(string-append "format=json&action=login&lgname=" (scheme-escape(d-GetUserName)) "&lgpassword=" (scheme-escape(d-GetPassword)) ))
	)

	; Actually logs you in and prepares a HTTP-Cookie you have to use in all other Media-Wiki Actions as third (d-HTTP) parameter.
	(define (CookieString) 
		(define LogMeInReturn (LogMeIn))
		
			; Raise Error. Sorry, I don't know how to make Blocks and if/else does only allow one statement.
			(define	(RaiseError)
			  (begin
			    (d-WarningDialog "Login Error - Please check your username and password in Edit->prefs->misc")
				(display "\nLogin Error - Please check your username and password in Denemos Preferences")
				;return CookieError
				(string-append "CookieError"))		
			)
			
			; Test if hostname is ok
			(if (string-ci=? LogMeInReturn "ERROR")
			(display "\nConnection Error - Server unavailable")
			
				;If Server is ok check Login-Data:
				(if  (string-ci=? (ParseJson LogMeInReturn "result") "Success")
						
				; If login is good go ahead and build the cookie string						
				(string-append 
				"Cookie: "(ParseJson LogMeInReturn "cookieprefix")"UserName=" (ParseJson LogMeInReturn "lgusername")
				"; "(ParseJson LogMeInReturn "cookieprefix")"UserID=" (ParseJson LogMeInReturn "lguserid") 
				"; "(ParseJson LogMeInReturn "cookieprefix")"Token=" (ParseJson LogMeInReturn "lgtoken")
				"; "(ParseJson LogMeInReturn "cookieprefix")"_session=" (ParseJson LogMeInReturn "sessionid") 
				"\n")
				
				;else
				(RaiseError)

				)
			)	
	)	

	; Prepare request Edit-Token.
	; First send d-HTTP, then parse the token, then modify it to the right format.
	(define (GetEditToken name CookieStringReturn)
		(define (ReceiveRawToken)
			(d-HTTP 
			HTTPHostname
			HTTPSite
			CookieStringReturn
			(string-append "format=json&action=query&prop=info|revisions&intoken=edit&titles="name))
		)	
		
		;json gives you +\\ @ Tokens end, but you need only +\ which is %2B%5C in url-endcoded format. 
		(string-append (string-trim-both (string-trim-both (ParseJson (ReceiveRawToken) "edittoken" ) #\\) #\+) "%2B%5C")
	)	
	
	;This will overwrite the page named like the parameter "name". If it is not existend it will be created.
	;Any OverwritePage call has to be made in (d-UploadRoutine)'s body.
	(define (OverwritePage CookieStringReturn)
		
		(define (GetLicenseAndBuildString)
			;(define license (d-GetUserInput "License" "Please choose a license for your script. For example GPL or LGPL" "GPL")) ; This is gone. Scripts have to be GPL, too.
			(define (SiteString) ; Any whitespace will be send, too.
(string-append 
"{{Script
|Name = " name "
|Author =  " (scheme-escape(d-GetUserName)) "  
|Label = " label  "
|License =  GPL 
|Explanation = " tooltip "
|SubLabel = " menupath "
|Version = " DENEMO_VERSION "
}}
=== Script ===			
<syntaxhighlight lang=\"scheme\">
" script "
</syntaxhighlight>
			
=== Initscript === 
<syntaxhighlight lang=\"scheme\">
" initscript "
</syntaxhighlight>
			
=== After === 
<syntaxhighlight lang=\"scheme\">
" after "
</syntaxhighlight>
")
			)
			
			;Send the data to let the API generate a new site!		
			(d-HTTP
				HTTPHostname
				HTTPSite
				CookieStringReturn
				(string-append "action=edit&title=" name "&format=json&summary=" tooltip "&text=" (SiteString) "&token=" (GetEditToken name CookieStringReturn))
			)	
			
			;Show script in browser
			(d-Help (string-append "http://" HTTPHostname "/index.php/" name))				
	
		); End of GetLicenseAndBuildString
	
	
	;check if Login/Building the Cookie was correct
	(if (string-ci=? CookieStringReturn "CookieError")
		(display "\nAn error occured while performing the task. Thats why the result of your Upload-Command is: ")
		(GetLicenseAndBuildString)
	)	
	


	);;;; End of OverwritePage 
	
	;;;; The real action happens here.	This is the only place where (CookieString) is called so we have only one Login at all.
	(display (OverwritePage (CookieString) )) ;show and execute

) ; End Of (d-UploadRoutine)


;;; play a note a mid-volume 80
(define (PlayNote pitch duration)
  (d-OutputMidiBytes (string-append "0x9$ " pitch " 80"))
 (d-OneShotTimer duration (string-append "(d-OutputMidiBytes " "\"" "0x8$ " pitch " 0" "\"" ")" )) 
  )

;;;;;;;;;;;;;; Refresh Procedures.
;;;;;;;;;;;;;; Naming convention D-<tag> is the refresh proc for tag

(define (D-Anacrusis)
(let ((duration (d-GetDurationInTicks)))
  (if (boolean? duration)
      (set! duration 0))
  (while (d-NextObjectInMeasure) 
	 (set! duration (+ duration (d-GetDurationInTicks))))
  (PrevDirectiveOfTag "Anacrusis")
  (set! duration (/ duration 12))
  (if (equal? 0 duration)
      (begin
	(HideStandaloneDirective))
      (begin
	(d-DirectivePut-standalone-postfix "Anacrusis" (string-append "\\partial 128*" (number->string duration) " " ))))))


;;;;;;;;;;;;;;;;;
(define (MeasureEmpty?) (equal? "None" (d-GetType)))

;;;;;;;;;;;;;;;;;;
(define* (GetPrevailingTimeSig #:optional (numberorstring #f) ) 
	(if numberorstring
		(string->number (d-InsertTimeSig "query=timesigname"))
		(d-InsertTimeSig "query=timesigname")
	))



;;;;;;;Get Measure Filling Status
(define (MeasureFillStatus)
(let script ((MaxTicks 0) (return #f))
(d-PushPosition)
(GoToMeasureEnd)
(set! MaxTicks (* 1536 (GetPrevailingTimeSig #t) )) ; How many ticks are in a 100% filled measure?

(set! return (cond 
 	((not(d-GetEndTick)) #f) ; empty
 	((< (d-GetEndTick) MaxTicks) #f) ; underful
 	((= MaxTicks (d-GetEndTick)) 1)  ; 100% filled
 	((< MaxTicks (d-GetEndTick)) 2) ; >100% filled
	(else  (display "strange!")) ; ?
 	))
  (d-PopPosition)
  return
))

(define (EmptyMeasure?)
  (not (d-GetEndTick)))

(define (UnderfullMeasure?)
  (or (EmptyMeasure?)
     (not (MeasureFillStatus))))

(define (FullDurationMeasure?)
  (and (not (UnderfullMeasure?))
       (= 1 (MeasureFillStatus))))

(define (OverfullMeasure?)
  (= 2 (MeasureFillStatus)))
  ;;(and (not (EmptyMeasure?)) 
 ;; (> (d-GetEndTick) (* 1536 (GetPrevailingTimeSig #t)))))

(define (MeasureComplete?) (or (FullDurationMeasure?) 
			       (d-Directive-standalone? "Anacrusis")
			       (d-Directive-standalone? "ShortMeasure")))

(define (Paste::MeasureBreakInClipboard?)
(let searchformeasurebreak ((counter 1))  ;start at the second position to avoid leading measurebreaks, which do not count. 
		(case (d-GetClipObjType 0 counter) 
		  ((#f) #f ) ; No object left
		  ((8) #t) ; Measurebreak found
		  (else (searchformeasurebreak (+ 1 counter)))
   		)))
   		
   		
;;;;;;;;;;;;;;;;;
(define (DenemoFirst)
  (begin
    (display "DenemoFirst")))


;;;;;;;;;;;;
(define (DenemoGoBack)
  (begin
    (d-AdjustPlaybackStart -1.0)
    (d-RefreshDisplay)))


(define (DenemoPrevious)
  (begin
    (d-AdjustPlaybackEnd -1.0)
    (d-RefreshDisplay)))


(define (DenemoRewind)
  (begin
    (display "DenemoRewind")))

(define (DenemoStop)
  (begin
    (set! Playback::Loop #f)
    (d-Stop)))

(define (DenemoPlay)
  (begin
    (d-Play "(display \"Here endeth a scripted playback\")")))

(define (DenemoPause)
  (begin
    (display "DenemoPause")))

(define (DenemoGoForward)
  (begin
    (d-AdjustPlaybackEnd 1.0)
    (d-RefreshDisplay)))


(define (DenemoNext)
  (begin
    (d-AdjustPlaybackStart 1.0)
    (d-RefreshDisplay)))



(define (DenemoForward)
  (begin
    (display "DenemoForward")))

(define (DenemoLast)
  (begin
    (display "DenemoLast")))


(define Playback::Loop #f)
(define (DenemoLoop)
  (begin
    (display "DenemoLoop")
    (set! Playback::Loop #t)
    (d-Play "(if Playback::Loop (DenemoLoop))")
))

(define DenemoTempo::Value 1.0)
(define (DenemoTempo)
  (begin
    (d-MasterTempo DenemoTempo::Value)))

(define DenemoVolume::Value 1.0)
(define (DenemoVolume)
  (begin
    (d-MasterVolume DenemoVolume::Value)))

    
(define (DenemoSetPlaybackStart)
  (begin
    (if (boolean? (d-GetMidiOnTime))
	(d-RecreateTimebase))
    (if (number? (d-GetMidiOnTime))
	(begin (d-SetPlaybackInterval (d-GetMidiOnTime) #t)
	(d-RefreshDisplay)))))

(define (DenemoSetPlaybackEnd)
  (begin
    (if (boolean? (d-GetMidiOffTime))
	(d-RecreateTimebase))
    (if (number? (d-GetMidiOffTime))
	(begin (d-SetPlaybackInterval #t (d-GetMidiOffTime))
	(d-RefreshDisplay)))))

(define (DenemoSetPlaybackIntervalToSelection)
  (begin
    
    (let ((start #f)(end #f))
      (set! end (d-GetMidiOffTime))
      (if (boolean? end)
	  (d-RecreateTimebase))
      (set! end (d-GetMidiOffTime))
      (if (boolean? end)
	  (d-WarningDialog "End the selection at a note")
	  (begin
	    (d-GoToMark)
	    (set! start (d-GetMidiOnTime))
	    (if (boolean? start)
		(d-WarningDialog "Start the selection at a note")
		(begin
		  (if (< end start)
		      (d-SetPlaybackInterval end start)
		      (d-SetPlaybackInterval start end))
		  (d-RefreshDisplay))))))))


(define (DenemoPrintAllHeaders)
  (let ((lily "printallheaders"))
    (if (d-CheckLilyVersion "2.11.59")
	(set! lily "print-all-headers"))
     (d-DirectivePut-paper-postfix "PrintAllHeaders" (string-append lily " = ##t\n"))))



;;;; Move right until "appending" or "none" which is the Measure End
(define (GoToMeasureEnd)

(let loop ()
    (if  (or (string-ci=?  (d-GetType) "none") (string-ci=?  (d-GetType) "appending"))
	#t
	(begin (d-MoveCursorRight) (loop))))
		
)


;;;;;;;;;;;;;;;;;;;; Paste by Nils Gey // 2010
; Multistaff-Pasting always adds the complete part AFTER the current measure. It will never paste into an existing measure, not even in empty ones. 
; Singlestaff-Pasting happens at the cursor position. If there are still notes in the current measure, after the cursor position, those will be shoved to the right and placed after the pasted section, no matter if the result produce underful or overful measures. No other measure gets modified other than the current one. Singlestaff-Pasting will fill any empty measure on its way, until a non-empty one is encountered. For snippet pasting (< one full measure) it will automatically create new measures if needed. 
(define (DenemoPaste)
(let pasteblock ((paste::break? (Paste::MeasureBreakInClipboard?)))

(d-UnsetMark)
(d-PushPosition) 

;Multi staff selection is only possible for complete measures, not for parts. But In this case the first thing that needs to happen before beginning pasting to a new staff (including the first one) is to create an empty measure.

(if (d-GetClipObjType 1 0)
	(d-InsertMeasureAfter) ; Multistaff
)

 ; Add an initial empty measure if pasting single-staff multi-measure and the current measure is already full

(if (and paste::break? (string-ci=? (d-GetType) "Appending")  (d-GetClipObjType 0 0) (not (d-GetClipObjType 1 0)) (MeasureFillStatus)   )
        (if (d-MoveToMeasureRight) ; End of Staff?
			(if (MeasureEmpty?) 
				#t
				(d-InsertMeasureBefore) ) 
			(d-InsertMeasureAfter)))


; If a user wants to paste a multi-measure section between two objects the remaining objects (after the cursor -> until the next barline) need to put in an extra measure. d-SplitMeasure takes care of that.
; The conditions that need to met are quite a few:
	(if (and 
	(d-GetClipObjType 0 0) ; Check if there is a clipboard at all. 
	(not (d-GetClipObjType 1 0)) ; Only for single-staff
	(not (or (string-ci=?  (d-GetType) "none") (string-ci=?  (d-GetType) "Appending"))) ; Check if its the right position to split. There must be notes left in the measure. GetType returns "none" if its an empty measure  or "Appending" if there are no objects left until the next measure.
       paste::break?  ; If there is no measurebreak there is no need to split
	) 
		(d-SplitMeasure)
		#f)


;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Main Loop ;;;;;; 
;;;;;;;;;;;;;;;;;;;;;;;

 (let loopy ((staff 0) (count 0))
  ;Pasty is the default command which just inserts the clipboard-object and increases the object-counter, but does not touch the staff-counter
  (define (pasty)
  	(d-PutClipObj staff count)
  	(set! count (+ 1 count))
  	(loopy staff count))

 ; Nextplease increases the object-counter without pasting anything.
  (define (nextplease)
	(set! count (+ 1 count))
  	(loopy staff count))
 
 ;Nextstaff inserts a new measure only in this new staff! It is not possible to add a measure in all staffs because its possible to copy&paste only 2 out of n staffs. In such cases it would lead to empty measure in other staffs. Afterwards reset the object-counter to zero and increase the staff-counter to start a new pasting round.
;First go to the initial cursor position, then move staff down.
 (define (nextstaff)
   (if (= staff 0)  
          (d-PopPushPosition) ; remember the  end-position of the cursor only for the first staff to return there after paste
 	  (d-PopPosition)
    )

   (if (d-MoveToStaffDown) 
	(begin	(d-InsertMeasureAfter)
			(d-PushPosition) 
			(set! count 0)
			(set! staff (+ 1 staff))
		  	(loopy staff count))
  	(begin  (d-PopPosition) (d-MoveCursorRight) (display "Paste: Not possible to go a staff down, pasting whats possible.\n") #f) ; Warn about not enough staffs to paste in and place the cursor after the pasted section (duplicate of case -1)
    )
  )
  
 (define (measurebreak)
 
   (if (d-GetClipObjType 1 0) ;Multistaff?
	(begin (d-InsertMeasureAfter) (nextplease))  ;Yes it is, just go on and paste
      
        (if  (and (not (d-GetClipObjType 1 0)) (= 8 (d-GetClipObjType 0 0)) (= 0 count)) ; User might have copied a leading measurebreak by accident. Ignore this. Else go on and try to detect empty measures or add measures.
		(nextplease)
		(if (d-MoveToMeasureRight) ; End of Staff?
			(if (MeasureEmpty?) (nextplease) (begin (d-InsertMeasureBefore) (nextplease))) ;
			(begin (d-InsertMeasureAfter) (nextplease))))
)) 

; In the end move cusor to the initial staff, after the pasted part to allow direct pasting and editing again.
 (define (endthis)
 	(if (> staff 0) 
		(begin (d-PopPosition) (d-PopPosition) ))
	
	;This was needed at some type, after an (unrelated?) upgrade of the script it was not needed anymore.  Better keep it in...
	;(if (d-GetClipObjType 0 0) (d-MoveCursorRight)) ; Only move cursor right if there was a clipboard to paste.
	(d-RefreshDisplay)
	#t
 )
  
  
 ; For clipboards smaller than one full measure Denemo will automatically add barlines if needed 
 (if (and (d-GetClipObjType staff count) (string-ci=?  (d-GetType) "Appending")  (not paste::break?) (MeasureFillStatus))
            (if (d-MoveToMeasureRight) ; End of Staff?
			(if (MeasureEmpty?) 
				#t
				(d-InsertMeasureBefore) ) 
			(d-InsertMeasureAfter)))
 
  
; The real action: Get the type of clipboard-object and decide what to do. In most cases it will be just pasting but someties special behaviour is needed. Because pasting a staffbreak does not actually moves the cursor to the new staff so this has to be done manually.
 (case (d-GetClipObjType staff count) 
	((#f) (endthis) ) ; No object left. Means "no clipboard", too.
	((-1) (display "No object")); should not happen anymore
	((0) (pasty)) ; note, rest, gracenote, chord
	((1) (pasty)) ;tuplet open
	((2) (pasty)) ;tuplet close
	((3) (pasty)) ; Clef
	((4) (pasty)) ;Timesignature
	((5) (pasty)) ;Keysignature
	((6) (display "barline"))
	((7) (pasty))	;stem directive
	((8) (measurebreak) ) ; Measurebreak
	((9) (nextstaff) ) ; staffbreak 
	((10) (display "dynamic"))	
	((11) (display "grace start")) ;deprecated
	((12) (display "grace end")) ;deprecated
	((13) (display "lyric")) ;deprecated
	((14) (display "figure")) 
	((15) (pasty)) ; Lilypond-Directive
	((16) (display "fakechord"))				
	((17) (display "partial"))
	(else (begin (display "Error! Object to paste unknown\n") #f))
   )
 ) 
 ))
 
 ;;;;;;;;;;;;;;;;;;;;;;;; Paste End

