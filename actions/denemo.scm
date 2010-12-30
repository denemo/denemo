(use-modules (srfi srfi-1))
(use-modules (srfi srfi-8))
(use-modules (srfi srfi-13))
(use-modules (ice-9 regex))
(use-modules (ice-9 optargs))
;Denemo subsystems and extra functions in additional files. 
(load "ans.scm") ; Abstract Note System for pitch calculations
(load "notationmagick.scm") ; Insert and modify, mostly randomized, music. Depends on ans.scm
(load "abstractionmovement.scm) ; Create an abstract form of the music in Scheme for further analysing. Depends on ans.scm 

;Needed to see if lyimport / mxml import is called from inside or outside Denemo
(define Denemo #t)

; Create a seed for (random int) one time Denemo starts. The seed is altered by random itself afterwards.
(let ((time (gettimeofday)))
    (set! *random-state*
        (seed->random-state (+ (car time)
                 (cdr time)))))

(define DenemoKeypressActivatedCommand #f);;;is true while a keyboard shortcut is invoking a script, unless the script has set it to #f


;;;;;; Helper functions for scripters
(define (blank)
	(system "clear"))

(define disp (lambda args
   (letrec ((disp-in (lambda (arg) 
              (if (null? arg) 
                  #f 
                  (begin 
                     (display (car arg)) 
                     (disp-in (cdr arg))))))) 
		     (disp-in args)
		     (newline))))


;;; GetUniquePairs is a function that takes a list and combines each value with any other, but without duplicates and in order.
;;; (a b c d) -> ab ac ad, bc bd, cd
(define (GetUniquePairs listy)
 (define returnList (list #f)) ; we need a non-empty list to append! to
 (define maxsteps (- (length listy) 1))
 (define (subMap memberA counter)
	(define subList '())
	(define (appendPair memberB)
		(append subList (cons memberA memberB)))
	(map appendPair (list-tail listy (+ 1 counter)))
 );subMap 

 (let loop ((counter 0))
  (if (= counter maxsteps)
	(list-tail returnList 1) ; get rid of the initial #f for the final return value
	(begin (append! returnList (subMap (list-ref listy counter) counter))   (loop (+ counter 1)))
  )
 )
); GetUniquePairs


;;;;;;;;;;;Get Lilypond Objects as strings. Currently just manual converting for single cases.
(define (GetContextAsLilypond) ; You will likely need (GetTypeAsLilypond) also. TODO: Create a real function!
"Staff"
)

(define (GetTypeAsLilypond)   ; You will likely need (GetContextAsLilypond) also. TODO: Replace with real information, derived from Lilypond
(define type (string->symbol (d-GetType)))
	(case type  ; Convert Denemo type to lilypond type
		((TIMESIG) "TimeSignature")	
		((CHORD) "NoteHead") ; Rests will be detected as CHORD but it will not work
		((KEYSIG) "KeySignature")
		((CLEF) "Clef")
		(else #f) 
	)
)


;;;;;;;;;; Prototype to insert Lilypond Directives. Wants a pair with car Tag and cdr lilypond: (cons "BreathMark" "\\breathe")
(define* (StandAloneDirectiveProto pair #:optional (step? #t) (graphic #f))
	(d-Directive-standalone (car pair))
	(d-DirectivePut-standalone-postfix (car pair) (cdr pair))
	(d-DirectivePut-standalone-minpixels (car pair) 30)
	(if graphic ;If the user specified a graphic use this, else greate a display text
		(begin (d-DirectivePut-standalone-graphic (car pair) graphic)
			   (d-DirectivePut-standalone-override (car pair) DENEMO_OVERRIDE_GRAPHIC))
		(d-DirectivePut-standalone-display (car pair) (car pair))
	)
	(if step?
		(d-MoveCursorRight)
	)
	(d-RefreshDisplay)
)


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
	  (if  (and (d-MoveToStaffDown) (d-IsInSelection))
		 (selection::MoveToStaffBeginning) ; there is a selection a staff down, loop to the beginning
		#f ; there is no staff down or the selection is single staff.
		)
	  #t ;NextSelecetedObject was succesful
	)
)


(define (selection::MoveToStaffBeginning)
(d-PushPosition)
 (if (d-GoToSelectionStart) ; Test if there is a selection at all
	(begin
		(d-PopPosition); return to the initial position to go to the correct staff
		(d-PushPosition); save it again in case that there is no selection in this staff.
		(d-MoveToBeginning)
		(let loop ()  ; Real work begins here. Loop through until you found it or end of staff.
		  (if (d-IsInSelection) 
		  	#t ; found the first note
			(if (d-NextObject) (loop) ; prevent endless loop if reaching the end of a staff without selection present
				(begin  (d-PopPosition) #f ))))  ; if end of staff and no selection return to initial position and return #f
	)
	(begin  (d-PopPosition) #f )   ; no selection at all. 
	) ; fi GoToSelectionStart
)


;Find the next object that returns #t from the given test function. Don't write the function in parentheses, just give the name (except you give a function that returns a name :))
(define (FindNextObjectAllStaffs test?) 
	(let loopy ()
	(if (d-NextObject)
		(if (test?)
			#t ; object found. Stop
			(loopy)) ; not the droids you're looking for, move on
		(if (d-MoveToStaffDown); no next object possible
			(begin (d-MoveToBeginning) ; lower staff found
				(if (test?)
					#t; object found. Stop
					(loopy))) ; first object of lower staff is not a member, start search again.
			#f) ; no staff left, final end.
	); if end
	);loopy end
)


;SingleAndSelectionSwitcher by Nils Gey Jan/2010
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Automatically applies a script to a whole selection. You can give different commands or command blocks with (begin) for single items or whole selections. You can enter a complete scheme script with (),  arguments and everything you would want to run standalone. Don't forget to escape chars like  \" . You can even use a complete (begin ) block.
;But attention! SingleAndSelectionSwitcher will still try to apply the given script to each of the single items alone. If you need a script which differs completly in beaviour for single/selection you have to write your own. You have to take out the (let loop () section for this and write your own selection part there.
;The applied script itself has to take care if the command can be applied to each potential item. If you want only notes/chords/rests you have to make sure the script does not abort on other objects. Its the same as giving proper return values for a single item, just return #f if a command is not possible for an item. While a single item just returns an error if you don't do it correctly, but does no harm otherwise, a script applied to a selection will stop on that item and leaves you on half on the way.
;Return values are the return values the script itself gives.
;The third, optional, parameter can prevent an object from be processed. By default this parameter is #t so the command will be will be applied to any object in the selection and let the command itself decide what to do (or just do nothing). By giving the third optional argument you can specify additional conditions, for example with GetType. In general: Insert test conditions here, if #t the current object will be processed, otherwise it will be skipped.

;Example: (SingleAndSelectionSwitcher  "(d-ChangeDurationByFactorTwo *)" "(d-ChangeDurationByFactorTwo *)")

(define* (SingleAndSelectionSwitcher commandsingle #:optional (commandselection commandsingle) (onlyFor "#t")) ; Amazingly commandsingle is already defined on spot so that it can be used again in the same line to define commandselection 
(d-PushPosition)
(if (and DenemoPref_applytoselection (d-GoToSelectionStart))
(begin
	(if (eval-string onlyFor)
		(eval-string  commandselection))
	(let loop ()
	(if (NextSelectedObjectAllStaffs)
	 	(if (eval-string onlyFor)
	 		(begin (eval-string  commandselection) (loop))
	 		(loop) ; don't process this object, next please.
	 	)
	))
	(d-GoToSelectionStart)
	(d-PopPosition)
	)
(begin	
	(eval-string commandsingle)	)
))
;; End of SingleAndSelectionSwitcher


;;; A set of simple tests / questions for score objects. 

(define (music?) 
  (if (string=? (d-GetType) "CHORD") #t #f))
	
(define (note?) 
  (if (and (string=? (d-GetType) "CHORD") (d-GetNoteName)) #t #f))

(define (rest?)
  (if (and (not (d-GetNoteName)) (string=? (d-GetType) "CHORD")) #t #f))

(define (chord?) 
  (if (note?)
	(if (string-contains (d-GetNotes) " ")
		#t
		#f
	)
  #f ; no note
))

(define (singlenote?) 
  (if (note?)
	(if (string-contains (d-GetNotes) " ")
		#f
		#t
	)
  #f ; no note
))
	
(define (directive?) 
  (if (string=? (d-GetType) "LILYDIRECTIVE") #t #f))

(define (timesignature?) 
  (if (string=? (d-GetType) "KEYSIG") #t #f))
  
(define (keysignature?) 
  (if (string=? (d-GetType) "TIMESIG") #t #f))
  
(define (clef?) 
  (if (string=? (d-GetType) "CLEF") #t #f))
  
(define (tupletmarker?) 
  (if (or (tupletopen?) (tupletclose?))  #t #f))
  
(define (tupletopen?) 
  (if (string=? (d-GetType) "TUPOPEN") #t #f))
  
(define (tupletclose?) 
  (if (string=? (d-GetType) "TUPCLOSE") #t #f))
 
(define (none?)
 (if (string=? (d-GetType) "None") #t #f))
	
(define (appending?)
 (if (string=? (d-GetType) "Appending") #t #f))	 
 
;;;;; End set of questions
		  

(define NextChordInSelection (lambda () (if (d-NextSelectedObject) 
					    (if (music?)
			                	 #t
			                	 (NextChordInSelection))
					    #f
					    )))
(define FirstChordInSelection (lambda () (if (d-GoToMark)
						  (if (music?)
			                	 #t)
						  #f)))
				    

(define ApplyToSelection (lambda (command positioning_command)
			   (begin
			     (if (eval-string positioning_command)
				 (begin
				   (d-PushPosition)
				   (eval-string  command)
				   (d-PopPosition)
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

(define (d-DirectiveDelete-standalone Tag)
(if (equal? (d-DirectiveGetTag-standalone) Tag)
(begin (d-DeleteObject) #t)
#f))

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


;;;;;;;;;;;;;;;;hardcode default number keys to Insert Note in Composer Mode
	(define wrap:Op0 (cons "(d-Insert0)" "(d-Insert0)" ))
	(define wrap:Op1 (cons "(d-Insert1)" "(d-Insert1)" ))
	(define wrap:Op2 (cons "(d-Insert2)" "(d-Insert2)" ))
	(define wrap:Op3 (cons "(d-Insert3)" "(d-Insert3)" ))
	(define wrap:Op4 (cons "(d-Insert4)" "(d-Insert4)" ))
	(define wrap:Op5 (cons "(d-Insert5)" "(d-Insert5)" ))
	(define wrap:Op6 (cons "(d-Insert6)" "(d-Insert6)" ))
	(define wrap:Op7 (cons "(d-Insert7)" "(d-Insert7)" ))
	(define wrap:Op8 (cons "(d-InsertBreve)" "(d-InsertBreve)"))
	(define wrap:Op9 (cons "(d-InsertLonga)" "(d-InsertLonga)"))

	

;;;;;;;;;;;;;;;; Double-Stroke for sequencing keypresses. By Nils Gey June 2010
;One parameter for the GUI-version or help window. This is the version that appears if someone clicks on the menu version.
;Ten optional parameters given as strings which can be only MENU commands: complete scheme syntax with (d-), but as string "" and with escaped \" in them. They return #f if not defined
;gui-version can be any command to aid the user. Most likely it will be a tooltip or better a GUI with radio buttons with all commands (if (not #f) ...) and help texts and maybe additional parameters.
;This script is ready to get paris as parameter. car is the command as string, cdr is a pretty name. However this is not compatible with d-GetOption, we need a better GUI for this.

(define* (doublestroke gui-version #:optional (first "#f") (second "#f") (third "#f") (fourth "#f") (fifth "#f") (sixth "#f") (seventh "#f") (eighth "#f") (ninth "#f") (tenth "#f"))

; Create a fallback-GUI which just lists all commands as radio-buttons. Used for the [space] variant.
(define (doublestroke::fallbackgui)
	(define doublestroke::result #f)
	
	(define (bo action) ; BuildOption
	    (if (pair? action)
			;User gave pairs for better documentation. Build the option 
			(string-append (car action) stop) 
			;User gave just strings or #f, build the option from this string		
			(if (or (not (string=? action "#f" )) (not action) )
				(string-append action stop) 
				""))
	);bo end 
		
	(if (not gui-version) ;just a small performance-tweak
		(begin
			(set! doublestroke::result (d-GetOption  (string-append (bo first)  (bo second)  (bo third)  (bo fourth)  (bo fifth)  (bo sixth)  (bo seventh)  (bo eighth)  (bo ninth)  (bo tenth)) ))
			(if doublestroke::result (eval-string doublestroke::result)))))

; Eval the command with testing if it's a pair or just a string
(define (doublestroke::evalcommand parameter)
	(if (pair? parameter)
		(eval-string (car parameter))
		(eval-string parameter)))
		
;Rebind a wrapper key, check if pair or string
(define (doublestroke::bind command parameter)
   (if (pair? parameter)
		(set-cdr! command (car parameter))
		(set-cdr! command parameter)))

		
; Short command to invoke the gui which tests if the author specified his own first.
(define (doublestroke::invokegui)
	(if gui-version (eval-string gui-version)
			 	 (doublestroke::fallbackgui)))			 	 		

; The real action. Wait for a keypress and decide what do with it afterwards, Space triggers the GUI, Return locks-in the commands and makes them permanent keybindings.
(if DenemoKeypressActivatedCommand
	(begin 
	(case (string->symbol (d-GetCommand))
		((d-OpOne)  (doublestroke::evalcommand first))
		((d-OpTwo)  (doublestroke::evalcommand second))
		((d-OpThree)  (doublestroke::evalcommand third))
		((d-OpFour)  (doublestroke::evalcommand fourth))
		((d-OpFive)  (doublestroke::evalcommand fifth))
		((d-OpSix)  (doublestroke::evalcommand sixth))
		((d-OpSeven)  (doublestroke::evalcommand seventh))
		((d-OpEight)  (doublestroke::evalcommand eighth))
		((d-OpNine)  (doublestroke::evalcommand ninth))
		((d-OpZero)  (doublestroke::evalcommand tenth))
		((d-UnsetMark)  (doublestroke::invokegui))
		((d-AddNoteToChord) (begin
				(doublestroke::bind wrap:Op1 first)
				(doublestroke::bind wrap:Op2 second)
				(doublestroke::bind wrap:Op3 third)
				(doublestroke::bind wrap:Op4 fourth)
				(doublestroke::bind wrap:Op5 fifth)
				(doublestroke::bind wrap:Op6 sixth)
				(doublestroke::bind wrap:Op7 seventh)
				(doublestroke::bind wrap:Op8 eighth)
				(doublestroke::bind wrap:Op9 ninth)
				(doublestroke::bind wrap:Op0 tenth)
				))
		(else #f))
	  (set! DenemoKeypressActivatedCommand #f))
	  
	 (doublestroke::invokegui) )) ; if not DenemoKeypressActivated

	; Example, where key 6 to 9 are not defined. The WarningDialog shows if you press [space] or invoke the command through the menu. 
	; (doublestroke "(d-WarningDialog \"After invoking the command, what you already have done right now, press a number key to specify number to print to the console or any other key to abort.\n\")" 
	; "(d-Finger1)"  "(d-Finger2)" "(d-Finger3)"  "(d-Finger4)"  "(d-Finger5)" "#f" "#f" "#f" "#f" "(d-Finger0)")
	

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
	(set! title (scheme-escape (d-GetUserInput (string-append type " " fieldname)
				    (string-append "Give a name for the " fieldname " of the " type) current))))
    (d-DirectivePut-header-override tag DENEMO_OVERRIDE_GRAPHIC)
    (d-DirectivePut-header-display tag (string-append type " " fieldname ": " (html-escape title)))
    
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
  (set! title (scheme-escape (d-GetUserInput (string-append "Score " field) 
			      (string-append "Give a name for the " field " of the whole score") current)))
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



;;;; GoToMeasureEnd: Move right until "appending" or "none" which is the Measure End
(define (GoToMeasureEnd)
  (let loop ()
    (if  (or (none?) (appending?))
	#t
	(begin (d-MoveCursorRight) (loop))))	
)

;;;; GoToMeasureBeginning
(define (GoToMeasureBeginning)
  (if (d-MoveToMeasureLeft)
	(d-MoveToMeasureRight)  
	(d-MoveToBeginning) 
  )
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

(if (and paste::break? (appending?)  (d-GetClipObjType 0 0) (not (d-GetClipObjType 1 0)) (MeasureFillStatus)   )
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
	(not (or (none?) (appending?))) ; Check if its the right position to split. There must be notes left in the measure. GetType returns "none" if its an empty measure  or "Appending" if there are no objects left until the next measure.
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
  
  
; For clipboards smaller than one full measure Denemo will automatically add a barline before an object if needed 
;;; The inline (or ...) is for the case that a non-note is at the start or end of clipboard. If its at the start the measure is allowed to break if pasting to a full measure. If its not the first item (e.g. the last) the measure is not allowed to break before. But not if the first one is the only one!

 
 (if (and  
	(d-GetClipObjType staff count) ; valid paste?
	(not (and ; Only breaks for more than one object in the clipboard
	 	  (not (d-GetClipObjType staff 1))
	 	  (= count 0)
 	))    
 	(or (= 0 (d-GetClipObjType staff count))   ; Only break before notes except its the first item in list
 	      (= count 0)
 	) 	 		       
 	(appending?)
 	(not paste::break?) 
 	(MeasureFillStatus)) ; if conditions end
 	
            (if (d-MoveToMeasureRight) ; End of Staff?
			(if (MeasureEmpty?) 
				#t
				(d-InsertMeasureBefore) ) 
			(d-InsertMeasureAfter))
	)
  
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
 
 ;;;; Shuffling Sequences
;;; http://mumble.net/~campbell/scheme/shuffle.scm
;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.
;;; This uses SRFIs 1 (list-lib) and 8 (receive).

;;;; Merge Shuffle
;;; Partition the list into two equal halves; shuffle the two halves,
;;; and then merge them by randomly choosing which half to select the
;;; next element from.

(define (flip-coin) 
	(if (= 1 (random 2))
		#t
		#f))


(define (merge-shuffle-list list)

  (define (merge a b)
    (cond ((not (pair? a)) b)
          ((not (pair? b)) a)
          (else
           (if (flip-coin)
               (cons (car a) (merge (cdr a) b))
               (cons (car b) (merge a (cdr b)))))))

  (define (partition list a b)
    (let ((next (cdr list))
          (a b)
          (b (cons (car list) a)))
      (if (null-list? next)
          (values a b)
          (partition next a b))))

  (if (null-list? list)
      '()
      (let shuffle ((list list))
        (if (null-list? (cdr list))
            list
            (receive (a b) (partition list '() '())
              (merge (shuffle a) (shuffle b)))))))

;;; This has *far* too many SET-CDR!s.

(define (merge-shuffle-list! list)

  (define (merge! a b)
    (cond ((null-list? a)       b)
          ((null-list? b)       a)
          ((flip-coin)          (%merge! a b) a)
          (else                 (%merge! b a) b)))

  (define (%merge! a b)
    (cond ((null-list? (cdr a))
           (set-cdr! a b))
          ((flip-coin)
           (%merge! (cdr a) b))
          (else
           (%merge! b (let ((next (cdr a)))
                        (set-cdr! a b)
                        next)))))

  (define (partition! list a b)
    (let ((next (cdr list)))
      (set-cdr! list a)
      (if (null-list? next)
          (values list b)
          (partition! next b list))))

  (if (null-list? list)
      '()
      (let shuffle! ((list list))
        (if (null-list? (cdr list))
            list
            (receive (a b) (partition! list '() '())
              (merge! (shuffle! a) (shuffle! b)))))))
;;;;;;;;;;;;;;; End Shuffle


(define (lyimport::load-file pathname filename)
  (load (string-append DENEMO_ACTIONS_DIR "lyimport.scm"))
  (set! lyimport::pathname pathname) 
  (set! lyimport::filename filename)
  (eval-string (lyimport::import)))

;;;;;;;;;;Duration Conversions between Denemo, Lilypond and Tick syntax.
;; A table of common values
; 6 = 256 = 8
;12 = 128 = 7
;24 = 64 = 6
;48 = 32 = 5
;96 = 16 = 4
;192 = 8 = 3
;384 =4 = 2
;768 = 2 = 1
;1536 = 1 = 0
;3072 = 0.5  = -1  ; Breve. 0.5 and -1 are not existend.  Lilypond and Denemo use the string breve instead.

; Guile returns a value with .0, which should be exact but internally it's inexact. So we need this little back and forth conversion hack.
(define (duration::inexact->exact return)
  (inexact->exact (string->number (number->string return)))
)

;Some functions, like Upbeat, know only a single tick-value. They need to guess the baseNote.
(define (duration::GuessBaseNoteInTicks ticks)
; first guess the basic note duration.  2*x > ticks < 1*x  is always true in our circumstance 
  (cond
	 ( (and (>= ticks 6) (< ticks 12)) 6 )       ;1/256
 	 ( (and (>= ticks 12) (< ticks 24))  12 )   ;1/128
 	 ( (and (>= ticks 24) (< ticks 48)) 24)   ;1/64
 	 ( (and (>= ticks 48) (< ticks 96)) 48 )   ;1/32
 	 ( (and (>= ticks 96) (< ticks 192))  96)   ;sixteen 1/16
 	 ( (and (>= ticks 192) (< ticks 384)) 192 ) ; eight 1/8
 	 ( (and (>= ticks 384) (< ticks 768))  384 ) ; quarter 1/4
 	 ( (and (>= ticks 768) (< ticks 1536))  768) ; half 1/2
 	 ( (and (>= ticks 1536) (< ticks 3072))  1536 ) ; whole 1
 	 ( (and (>= ticks 3072) (< ticks 6144))  3072 ) ; breve 2*1
 	 ( (and (>= ticks 6144) (< ticks 12288))  6144) ; longa 4*1
 	 ( (and (>= ticks 12288) (< ticks 24576))  12288 )  ; maxima 8*1
	(else #f)
))

; Calculate a new Duration in Ticks with a basic note value and a number of augmentation-dots
(define (duration::CalculateTicksWithDots baseTicks numberOfDots)
	; x = basic note value
	; n = number of dots
	; 2x - x/2^n 
	(-  (* 2 baseTicks) (/ baseTicks (expt 2 numberOfDots)))
)

; Calculate how many dots a tick value has. Needs base duration, too.
(define (duration::CalculateDotsFromTicks ticks base)
; x = base , y = ticks. result is the number of dots
; log(x/(2*x-y))  / log(2)
 (define return (/  (log (/ base (- (* base 2) ticks)))   (log 2) ))
 (duration::inexact->exact return)
)

; Get Number of Dots from a Lilypond string like "2.". Its so basic it will work on Denemo notes, too.
(define (duration::GetNumberOfDotsInLilypond input)
 (length (cdr (string-split input #\.)))
)

; For the sake of completenes. Denemo and Lilypond dot-syntax is just the same, only the number itself is different.
(define (duration::GetNumberOfDotsInDenemo input)
	(duration::GetNumberOfDotsInLilypond input)
)

(define (duration::denemo->lilypond number)
	(define return (expt 2 number))
	return
)

(define (duration::lilypond->denemo number)
	(define return (/ (log number) (log 2))	)
	 (duration::inexact->exact return)
)

(define (duration::denemo->ticks number) ; increases with negative integers
	(define return (* (expt 2 (- 8 number)) 6))
	return
)

(define (duration::lilypond->ticks number) ; increases with 0.5, 0.25 etc.
	(define return (* (expt 2 (- 8 (/ (log number) (log 2)))) 6))
	 (duration::inexact->exact return)
)

; Ticks->Denemo wants a number but returns a string because of dots
(define* (duration::ticks->denemo number #:optional (basenumber number))
 (define numberOfDots  (duration::CalculateDotsFromTicks number basenumber))
;n = -(log(y/3)-9*log(2))/log(2) 
 (define return (- (/ (- (log (/ basenumber 3)) (* 9 (log 2))) (log 2))))
 (set! return (duration::inexact->exact return))
 (string-append (number->string return) (string-concatenate (make-list numberOfDots "."))) 
)

;Ticks->Lilypond wants a number but returns a string because of dots
(define* (duration::ticks->lilypond number #:optional (basenumber number))
 ;Equation in readable form: http://www.numberempire.com/equationsolver.php?function=y+%3D+6*2^%288-n%29&var=n&answers=
 (define numberOfDots  (duration::CalculateDotsFromTicks number basenumber))
 (define return  (expt 2 (- (/ (- (log (/ basenumber 3)) (* 9 (log 2))) (log 2)))))
 (set! return (duration::inexact->exact return))
 (string-append (number->string return) (string-concatenate (make-list numberOfDots "."))) 
)
;;;;;;;;;; End of duration-conversion


;;;;;;;; Applied duration scripts
(define (duration::GetNumberOfDotsInTicks) ; Fails for tuplets
	 (duration::CalculateDotsFromTicks (d-GetDurationInTicks) (duration::GetBaseDurationInTicks))
)
	   

(define (duration::GetBaseDurationInTicks)
	(define ticks (d-GetBaseDurationInTicks))
	(if ticks
		(abs ticks)
		#f)
)
	   

(define (duration::GetSelectionDurationInTicks) 
  (d-PushPosition)
  (if (d-GoToSelectionStart)
	(let loop ((ticks 0))
		(set! ticks (+ ticks (d-GetDurationInTicks)))
		(if (d-NextSelectedObject)
			(loop ticks)
			(begin  (d-PopPosition) ticks)
		)
	)
	#f ; no selection
    )

)

(define* (duration::ChangeNoteDurationInTicks ticks #:optional (dots 0))
; First change the base-duration. The d-Change commands also delete any dots.
  (define (changeBase number) (case number
   ;((12288) (d-ChangeMaxima))  ;Maxima
   ((6144)	(d-ChangeLonga)) ; Longa
   ((3072)	(d-ChangeBreve)) ; Breve
   ((1536)	(d-Change0)) ; Whole
   ((768)	(d-Change1)) ; Half
   ((384)	(d-Change2)) ; Quarter
   ((192)	(d-Change3)) ; Eighth
   ((96)	(d-Change4)) ; Sixteenth
   ((48)	(d-Change5)) ; 1/32
   ((24)	(d-Change6)) ; 1/64
   ((12)	(d-Change7)) ; 1/128
   ((6)		(d-Change8)) ; 1/256
   (else   #f )
 ))
 
 
; Second step: add dots
  ; d-ChangeN work on appending position, but Breve and Longa not. But d-AddDot works on appending, too. So we must rule Appending out, else it will add dots without changing the duration for breve and longa.
  (if (and (music?) (integer? ticks) (integer? dots) (changeBase ticks)) ; <-- the action changeBase itself needs to be a test, too. 
  (let loop ((i 0))
	(if (= dots i)
	#t
	(begin
	   (d-AddDot)  
	   (loop (+ i 1)))))
  #f)
)



;;;;;;;;;;;;;;


(define MIDI-shortcuts::alist '(("" . "")))
(define (SetMidiShortcut shortcut command)
	(set! MIDI-shortcuts::alist (assoc-set! MIDI-shortcuts::alist shortcut command)))

(SetMidiShortcut "FootpedalUp" #f)
(SetMidiShortcut "FootpedalDown" #f)

(define (MIDI-shortcut::controller type value)
;;(format #t "controller type ~a value ~a\n" type value)
  (cond ((and (equal? type 64) (equal? value 127))
    	  (assoc-ref MIDI-shortcuts::alist "FootpedalUp"))
        ((and (equal? type 64) (equal? value 0))
    	  (assoc-ref MIDI-shortcuts::alist "FootpedalDown"))

        ((equal? type 1)
	 (let ((thestep  (round(/ (- value 64) 16))))
	 (PlayNote  
	  (number->string  (+ 60 (* 4 thestep) ))
	  100)
	     
	 (eval-string (string-append "(d-SetEnharmonicPosition " (number->string  thestep) ")"))
         #f)
        )


      (else #f)))


(define Pitchbend::commandUp "(d-CursorRight)")
(define Pitchbend::commandDown "(d-CursorLeft)")
(define  Pitchbend::timer 0)
(define (MIDI-shortcut::pitchbend value)
  ;(format #t "pitch bend value ~a\n" value)
  (cond ((> value (+ 50 8192))
	 (d-KillTimer Pitchbend::timer)
	 (if  Pitchbend::commandUp
	 (eval-string Pitchbend::commandUp)
	 (set! Pitchbend::timer (d-Timer 100 Pitchbend::commandUp)))
	 #f)

         ((< value (- 8192 50))
	  (d-KillTimer Pitchbend::timer)
	  (if  Pitchbend::commandDown
	  (eval-string Pitchbend::commandDown)
	  (set! Pitchbend::timer (d-Timer 100 Pitchbend::commandDown)))
	  #f)
	 (else  (d-KillTimer Pitchbend::timer) (set! Pitchbend::timer 0) #f )))
;;;;;;;;;;;;;;;;;;
;;;;;;;
(define (ForAllMovements script)
(d-PushPosition)
  (while (d-PreviousMovement)
	 #t)
  (eval-string script) 	
  (while (d-NextMovement)
	(eval-string script))
(d-PopPosition))
;;;;;;;

(define (d-GetStartTick)
	(- (d-GetEndTick) (d-GetDurationInTicks)))
	
; from http://icem.folkwang-hochschule.de/~finnendahl/cm_kurse/doc/t-y-scheme/t-y-scheme-Z-H-7.html
; needed for define-macro defstruct
(define list-position
  (lambda (o l)
    (let loop ((i 0) (l l))
      (if (null? l) #f
          (if (eqv? (car l) o) i
              (loop (+ i 1) (cdr l)))))))

; from http://www.ccs.neu.edu/home/dorai/t-y-scheme/t-y-scheme-Z-H-11.html#node_sec_9.2
(define-macro defstruct
  (lambda (s . ff)
    (let ((s-s (symbol->string s)) (n (length ff)))
      (let* ((n+1 (+ n 1))
             (vv (make-vector n+1)))
        (let loop ((i 1) (ff ff))
          (if (<= i n)
            (let ((f (car ff)))
              (vector-set! vv i 
                (if (pair? f) (cadr f) '(if #f #f)))
              (loop (+ i 1) (cdr ff)))))
        (let ((ff (map (lambda (f) (if (pair? f) (car f) f))
                       ff)))
          `(begin
             (define ,(string->symbol 
                       (string-append "make-" s-s))
               (lambda fvfv
                 (let ((st (make-vector ,n+1)) (ff ',ff))
                   (vector-set! st 0 ',s)
                   ,@(let loop ((i 1) (r '()))
                       (if (>= i n+1) r
                           (loop (+ i 1)
                                 (cons `(vector-set! st ,i 
                                          ,(vector-ref vv i))
                                       r))))
                   (let loop ((fvfv fvfv))
                     (if (not (null? fvfv))
                         (begin
                           (vector-set! st 
                               (+ (list-position (car fvfv) ff)
                                  1)
                             (cadr fvfv))
                           (loop (cddr fvfv)))))
                   st)))
             ,@(let loop ((i 1) (procs '()))
                 (if (>= i n+1) procs
                     (loop (+ i 1)
                           (let ((f (symbol->string
                                     (list-ref ff (- i 1)))))
                             (cons
                              `(define ,(string->symbol 
                                         (string-append
                                          s-s "." f))
                                 (lambda (x) (vector-ref x ,i)))
                              (cons
                               `(define ,(string->symbol
                                          (string-append 
                                           "set!" s-s "." f))
                                  (lambda (x v) 
                                    (vector-set! x ,i v)))
                               procs))))))
             (define ,(string->symbol (string-append s-s "?"))
               (lambda (x)
                 (and (vector? x)
                      (eqv? (vector-ref x 0) ',s))))))))))
                      
; Create a music-object that holds various information. This is the smallest, single object 
(defstruct musobj pitch measure start duration end)
(define (createMusObj) 
	(make-musobj 'pitch (ANS::GetChordNotes)
				 'measure (d-GetMeasureNumber)
				 'start (d-GetStartTick)
				 'duration (d-GetDurationInTicks)
				 'end (d-GetEndTick)
							))							
;Examples:
;(define testob (createMusObj))							
;(set!musobj.duration testob 256)
;(display (musobj.start testob))
