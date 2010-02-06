(use-modules (srfi srfi-13))
(use-modules (ice-9 regex))
(use-modules (ice-9 optargs))


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


;SingleAndSelectionSwitcher by Nils Gey Jan/2010
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Automatically applies a script to a whole selection. You can give different commands or command blocks with (begin) for single items or whole selections. You can enter a complete scheme script with (),  arguments and everything you would want to run standalone. Don't forget to escape chars like  \" . You can even use a complete (begin ) block.
;But attention! SingleAndSelectionSwitcher will still try to apply the given script to each of the single items alone. If you need a script which differs completly in beaviour for single/selection you have to write your own. You have to take out the (let loop () section for this and write your own selection part there.
;The applied script itself has to take care if the command can be applied to each potential item. If you want only notes/chords/rests you have to make sure the script does not abort on other objects. Its the same as giving proper return values for a single item, just return #f if a command is not possible for an item. While a single item just returns an error if you don't do it correctly, but does no harm otherwise, a script applied to a selection will stop on that item and leaves you on half on the way.
;Return values are the return values the script itself gives.
;Example: (SingleAndSelectionSwitcher  "(d-ChangeDurationByFactorTwo *)" "(d-ChangeDurationByFactorTwo *)")

(define (SingleAndSelectionSwitcher commandsingle commandselection)

(if (and DenemoPref_applytoselection (d-GoToSelectionStart))
(begin
	(eval-string  commandselection)
	(let loop ()
	(if (d-NextSelectedObject)
	 	(begin (eval-string  commandselection) (loop))
	))
	(d-GoToSelectionStart)
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

(define (PlayNote pitch duration)
  (d-OutputMIDI (string-append "0x9$ " pitch " %%%"))
 (d-OneShotTimer duration (string-append "(d-OutputMIDI " "\"" "0x8$ " pitch " %%%" "\"" ")" )) 
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

(define (DenemoPlay)
  (begin
    (d-Play "(display \"Here endeth a scripted playback\")")))