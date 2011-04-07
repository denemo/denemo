(use-modules (srfi srfi-1)) ; List library
(use-modules (srfi srfi-8)) ; Returning and Accepting Multiple Values
(use-modules (srfi srfi-13)) ; String library
(use-modules (ice-9 regex)) ; regular expressions
(use-modules (ice-9 optargs)) ; optional (define* ) arguments
(use-modules (ice-9 q)) ; queue module
;Denemo subsystems and extra functions in additional files. 
(define (LoadDenemo file)
	 (load file))
	 ;(load (string-append DENEMO_ACTIONS_DIR file)))	 
	 
(LoadDenemo "ans.scm") ; Abstract Note System for pitch calculations
(LoadDenemo "notationmagick.scm") ; Insert and modify, mostly randomized, music. Depends on ans.scm
(LoadDenemo "abstractionmovement.scm") ; Create an abstract form of the music in Scheme for further analysing. Depends on ans.scm 
(LoadDenemo "commandlist.scm") ; Provide scrolling up and down through a list of commands. An extended toggle through multiple states.
(LoadDenemo "helpsystem.scm") ; An online help system to display text in the second status bar

;Needed to see if lyimport / mxml import is called from inside or outside Denemo
(define Denemo #t)

; Create a seed for (random int) one time Denemo starts. The seed is altered by random itself afterwards.
(let ((time (gettimeofday)))
    (set! *random-state*
        (seed->random-state (+ (car time)
                 (cdr time)))))

(define DenemoKeypressActivatedCommand #f);;;is true while a keyboard shortcut is invoking a script, unless the script has set it to #f


;Blank clears the console output. Don't use in released scripts, only for debugging.
(define (Blank)
	(system "clear"))

;disp is an advanced display. Just give anything you want to print, it appends strings automatically and does a line break in the end. Don't use in released scripts, only for debugging.
(define disp (lambda args
   (letrec ((disp-in (lambda (arg) 
              (if (null? arg) 
                  #f 
                  (begin 
                     (display (car arg)) 
                     (disp-in (cdr arg))))))) 
		     (disp-in args)
		     (newline))))

;Doublequote constant to avoid backslashing		     
(define DBLQ "\"")
;Linefeed constant to avoid backslashing		     
(define LFEED "\n")

; A function that returns #f for cases where commands work with chunks of code. this prevents the spamming of (lambda () #f) for a function that returns #f.
(define (False) 
	#f)
	
; A function that returns #t. See (False)
(define (True) 
	#t)

;repeat executes a proc n times
(define (Repeat proc n)
	(let loop ((counter 0))
		(if (= n counter)
			#t
			(begin
				(proc)
				(loop (1+ counter))))))

;Repeat a command until it returns #f
;Warning: Functions that do not return #f create infinity loops!
(define (RepeatUntilFail proc)
	(let loop ()
		(if (proc)
			(loop)
			#t)))
			
;Repeat a function while another (a test) returns #t. The return value of proc does NOT matter
;;Warning: From all Repeat functions this one has the highest probability to be stuck in a loop forever. Always use tests that MUST return #f in the end. Do NOT use the Denemo tests like (None?) or (Music?) for example, they know nothing about a staffs end.
(define (RepeatProcWhileTest proc test)
	(RepeatUntilFail 		
		(lambda () 
			(if (test)
				(begin (proc) #t); this is a dumb script. It will try to execute proc again even if proc itself returned #f. 	
				#f )))) ; test failed, let RepeatUntilFail fail.		


;;; GetUniquePairs is a function that takes a list and combines each value with any other, but without duplicates and in order.
;;; (a b c d) -> ab ac ad, bc bd, cd
(define (GetUniquePairs listy)
	 (define returnList (list #f)) ; we need a non-empty list to append! to
	 (define maxsteps (- (length listy) 1))
	 (define (subMap memberA counter)
		(define subList '())
		(define (appendPair memberB)
			(append subList (cons memberA memberB)))
		(map appendPair (list-tail listy (+ 1 counter))));subMap 

	 (let loop ((counter 0))
	  (if (= counter maxsteps)
		(list-tail returnList 1) ; get rid of the initial #f for the final return value
		(begin (append! returnList (subMap (list-ref listy counter) counter))   (loop (+ counter 1)))))); GetUniquePairs

;Get Lilypond Objects as strings. Currently just manual converting for single cases.
(define (GetContextAsLilypond) ; You will likely need (GetTypeAsLilypond) also. TODO: Create a real function!
"Staff")

(define (GetTypeAsLilypond)   ; You will likely need (GetContextAsLilypond) also. TODO: Replace with real information, derived from Lilypond
(define type (string->symbol (d-GetType)))
	(case type  ; Convert Denemo type to lilypond type
		((TIMESIG) "TimeSignature")	
		((CHORD) "NoteHead") ; Rests will be detected as CHORD but it will not work
		((KEYSIG) "KeySignature")
		((CLEF) "Clef")
		(else #f)))


; Prototype to insert Lilypond Standalone Directives. Wants a pair with car Tag and cdr lilypond: (cons "BreathMark" "\\breathe")
(define* (StandAloneDirectiveProto pair #:optional (step? #t) (graphic #f))
	(d-Directive-standalone (car pair))
	(d-DirectivePut-standalone-postfix (car pair) (cdr pair))
	(d-DirectivePut-standalone-minpixels (car pair) 30)
	(if graphic ;If the user specified a graphic use this, else greate a display text
		(begin (d-DirectivePut-standalone-graphic (car pair) graphic)
			   (d-DirectivePut-standalone-override (car pair) DENEMO_OVERRIDE_GRAPHIC))
		(d-DirectivePut-standalone-display (car pair) (car pair)))
	(if step?
		(d-MoveCursorRight))
	(d-RefreshDisplay))


; create documentation for a command - this version just prints out basic info
;;DocumentCommand
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


;Replace a part of a string
(define (Replace-nth list n elem)
  (cond 
    ((null? list) ())
    ((eq? n 0) (cons elem (cdr list)))
    (#t (cons(car list) (replace-nth (cdr list) (- n 1) elem)))))

; Get highest and lowest note as lilypond syntax. Works on Chords and Single Notes.
;; GetNotes returns a string of lily-notes from low to high. Make a list out of them and refer to the first (0) element or last (length -1) one.
;; Returns #f if not a chord
(define (GetLowestNote)
	(if (Note?)
	 (list-ref (string-tokenize(d-GetNotes)) 0 )
	 #f))

(define (GetHighestNote)
	(if (Note?)
	 (list-ref (reverse (string-tokenize(d-GetNotes))) 0)
	 #f))


;Next object in selection for all staffs
(define (NextSelectedObjectAllStaffs)
	(define lastposition (GetPosition))
	(if (and (d-MarkStatus) (d-IsInSelection))
		(if (d-NextSelectedObject)
		  #t ; found one. End
		   (if (d-MoveToStaffDown) ; no selected item left in the current staff. check one down.
		  		(if (selection::MoveToStaffBeginning) 
				  	#t ; found a selection in the lower staff
				  	(begin (apply d-GoToPosition lastposition ) #f)) ; reset cursor to the last known selection position and end.
				  #f)) ; no staff below
	  #f)); no selection or cursor not in selection

(define (selection::MoveToStaffBeginning)
	(define staffPosition (d-GetStaff))
	(define rememberPosition (GetPosition))
	(if (d-GoToSelectionStart)
		(begin 
			(d-GoToPosition #f staffPosition #f 1)
			(if (d-IsInSelection)
				#t
				(begin (apply d-GoToPosition rememberPosition) #f))) ; something went wrong. 
		#f)) ; no selection at all. 

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
	));loopy end


;SingleAndSelectionSwitcher by Nils Gey Jan/2010
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Automatically applies a script to a whole selection. You can give different commands or command blocks with (begin) for single items or whole selections. You can enter a complete scheme script with (),  arguments and everything you would want to run standalone. Don't forget to escape chars like  \" . You can even use a complete (begin ) block.
;But attention! SingleAndSelectionSwitcher will still try to apply the given script to each of the single items alone. If you need a script which differs completly in beaviour for single/selection you have to write your own. You have to take out the (let loop () section for this and write your own selection part there.
;The applied script itself has to take care if the command can be applied to each potential item. If you want only notes/chords/rests you have to make sure the script does not abort on other objects. Its the same as giving proper return values for a single item, just return #f if a command is not possible for an item. While a single item just returns an error if you don't do it correctly, but does no harm otherwise, a script applied to a selection will stop on that item and leaves you on half on the way.
;Return values are the return values the script itself gives.
;The third, optional, parameter can prevent an object from be processed. By default this parameter is #t so the command will be will be applied to any object in the selection and let the command itself decide what to do (or just do nothing). By giving the third optional argument you can specify additional conditions, for example with GetType. In general: Insert test conditions here, if #t the current object will be processed, otherwise it will be skipped.
;Example: (SingleAndSelectionSwitcher d-AddDot d-ToggleStaccato) ; this is nonsense, but valid. It will add a dot for single notes or toggle staccato for the whole selection.

(define* (SingleAndSelectionSwitcher commandsingle #:optional (commandselection commandsingle) (onlyFor True)) ; Amazingly commandsingle is already defined on spot so that it can be used again in the same line to define commandselection 
	(if (string? commandsingle) ; support for old scripts. They passed the complete string of scheme as parameter
		(set! commandsingle (eval-string (string-append "(lambda () " commandsingle " )"))))
	(if (string? commandselection)
		(set! commandselection (eval-string (string-append "(lambda () " commandselection " )"))))		
	(if (and DenemoPref_applytoselection (d-MarkStatus))
		(ForEachToSelection commandselection onlyFor)
		(commandsingle)))

; MapToSelection is like schemes (map) mixed with ApplyToSelection. Use a proc on all selection items and gather all proc return values in a list. You can give an optional test, only items which return #t are processed.
(define* (MapToSelection proc #:optional (onlyFor True))
	(define return (list #f))	; prepare return list
	(define (gather)
		(if (onlyFor) ; test the current item
			(append! return (list (proc))) ; execute the proc and append its return value as listmember to the returnlist
			#f))
	(if (and DenemoPref_applytoselection (d-MarkStatus)) ; only if preferences allow it and if there is a selection at all
		(begin 
			(d-PushPosition)
			(d-GoToSelectionStart)
			(gather) ; start one without selection testing. We already know we have a selection and RepeatProcWhileTest tests first which results in ignoring the first selected item.
			(RepeatProcWhileTest gather NextSelectedObjectAllStaffs) ; Use the proc/gather function on all items in the selection
			(d-PopPosition)
			(list-tail return 1))			
		#f))

;ForEachToSelection applies the command to each item in the selection. The return value is unspecified. Faster than MapToSelection.
(define* (ForEachToSelection proc #:optional (onlyFor True))
	(define (apply)
		(if (onlyFor) ; test the current item
			(proc)			
			#f))
	(if (and DenemoPref_applytoselection (d-MarkStatus)) ; only if preferences allow it and if there is a selection at all
		(begin 
			(d-PushPosition)
			(d-GoToSelectionStart)
			(apply) ; start one without selection testing. We already know we have a selection and RepeatProcWhileTest tests first which results in ignoring the first selected item.
			(RepeatProcWhileTest apply NextSelectedObjectAllStaffs) ; Use the proc/gather function on all items in the selection
			(d-PopPosition)
			(if #f #f) ; return unspecified.
			)			
		#f))


; A set of simple tests / questions for score objects. 
(define (Music?) 
  (if (string=? (d-GetType) "CHORD") #t #f))
	
(define (Note?) 
  (if (and (string=? (d-GetType) "CHORD") (d-GetNoteName)) #t #f))

(define (Rest?)
  (if (and (not (d-GetNoteName)) (string=? (d-GetType) "CHORD")) #t #f))

(define (Chord?) 
  (if (Note?)
	(if (string-contains (d-GetNotes) " ")
		#t
		#f
	)
  #f)) ; no note

(define (Singlenote?) 
  (if (Note?)
	(if (string-contains (d-GetNotes) " ")
		#f
		#t
	)
  #f)) ; no note
	
(define (Directive?) 
  (if (string=? (d-GetType) "LILYDIRECTIVE") #t #f))

(define (Timesignature?) 
  (if (string=? (d-GetType) "KEYSIG") #t #f))
  
(define (Keysignature?) 
  (if (string=? (d-GetType) "TIMESIG") #t #f))
  
(define (Clef?) 
  (if (string=? (d-GetType) "CLEF") #t #f))
  
(define (Tupletmarker?) 
  (if (or (Tupletopen?) (Tupletclose?))  #t #f))
  
(define (Tupletopen?) 
  (if (string=? (d-GetType) "TUPOPEN") #t #f))
  
(define (Tupletclose?) 
  (if (string=? (d-GetType) "TUPCLOSE") #t #f))
 
(define (None?)
 (if (string=? (d-GetType) "None") #t #f))
	
(define (Appending?)
 (if (string=? (d-GetType) "Appending") #t #f))	 
  
;;;;; End set of questions
		  

(define NextChordInSelection (lambda () (if (d-NextSelectedObject) 
					    (if (Music?)
			                	 #t
			                	 (NextChordInSelection))
					    #f)))
(define FirstChordInSelection (lambda () (if (d-GoToMark)
						  (if (Music?)
			                	 #t)
						  #f)))
				    
;;;Deprecated!
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


;Composer Mode hardcoded default number keys to "Insert Note"
;;car is the default value, cdr is the current one. Various scripts alter the cdr to temporarily change the number keys.
(define wrap:Op0 (cons d-Insert0 d-Insert0))
(define wrap:Op1 (cons d-Insert1 d-Insert1))
(define wrap:Op2 (cons d-Insert2 d-Insert2))
(define wrap:Op3 (cons d-Insert3 d-Insert3))
(define wrap:Op4 (cons d-Insert4 d-Insert4))
(define wrap:Op5 (cons d-Insert5 d-Insert5))
(define wrap:Op6 (cons d-Insert6 d-Insert6))
(define wrap:Op7 (cons d-Insert7 d-Insert7))
(define wrap:Op8 (cons d-InsertBreve d-InsertBreve))
(define wrap:Op9 (cons d-InsertLonga d-InsertLonga))


;;;;;;;;;;;;;;;; Double-Stroke for sequencing keypresses. By Nils Gey June 2010
;One parameter for the GUI-version or help window. This is the version that appears if someone clicks on the menu version.
;Ten optional parameters, each a pair with car = Pretty String for fallback-gui, cdr = scheme-command. Give (cons "" False) to skip over one slot.
;gui-version can be #f to generate a gui, or any command to be executed from the menu or if the gui/help is invoked.
(define* (Doublestroke gui-version #:optional (first (cons "" False)) (second (cons "" False)) (third (cons "" False)) (fourth (cons "" False)) (fifth (cons "" False)) (sixth (cons "" False)) (seventh (cons "" False)) (eighth (cons "" False)) (ninth (cons "" False)) (tenth (cons "" False)))
	;Rebind a wrapper key, check if pair or string
	(define (Bind command parameter)
		(set-cdr! command (cdr parameter)))
			
	; Short command to invoke the gui which tests if the author specified his own first.
	(define (doublestroke::invokegui)
		(if gui-version
			 (gui-version)
			 (begin FallBack ; create a gui from the given parameters, test for cancel-button #f
				(set! gui-version (apply RadioBoxMenu (delete (cons "" False) (list first second third fourth fifth sixth seventh eighth ninth tenth)))) 
				(if gui-version 
					(gui-version) ; execute the returned command
					#f)))) ; cancel-button, abort the process					
	
	(define (doublestroke::showhelp lockin?)
		(define helpstring "")
		(define (build parameter numberstring)
			(if (equal? (car parameter) "")
				""
				(string-append "[" numberstring "]" (car parameter) "  ")))
		(set! helpstring (string-append 
			(if lockin?
				"[Esc]Reset keys  "
				"[Space]Show GUI  [Enter]Lock keys in  ")			
			(build first "1")
			(build second "2")
			(build third "3")
			(build fourth "4")
			(build fifth "5")
			(build sixth "6")
			(build seventh "7")
			(build eighth "8")
			(build ninth "9")
			(build tenth "0")
			(if lockin? "" "[Other]Abort")))
		(if lockin?
			(Help::Push (cons 'doublestroke helpstring))
			(Help::Push (cons 'doublestroketemp helpstring))))
				
	; The real action. Wait for a keypress and decide what do with it afterwards, UnsetMark triggers the GUI, AddNoteToChord locks-in the commands and makes them permanent keybindings.
	(if DenemoKeypressActivatedCommand
	  (begin
		(doublestroke::showhelp #f) 
		(case (string->symbol (d-GetCommand))
			((d-OpOne)  (begin ((cdr first)) (Help::Pop))) ; Execute command then remove help-text and show the one before it.
			((d-OpTwo) (begin ((cdr second))(Help::Pop)))
			((d-OpThree)  (begin ((cdr third))(Help::Pop)))
			((d-OpFour)  (begin ((cdr fourth))(Help::Pop)))
			((d-OpFive)  (begin ((cdr fifth))(Help::Pop)))
			((d-OpSix)  (begin ((cdr sixth))(Help::Pop)))
			((d-OpSeven)  (begin ((cdr seventh))(Help::Pop)))
			((d-OpEight)  (begin ((cdr eighth))(Help::Pop)))
			((d-OpNine)  (begin ((cdr ninth))(Help::Pop)))
			((d-OpZero)  (begin ((cdr tenth))(Help::Pop)))
			((d-UnsetMark)  (begin  (doublestroke::invokegui)(Help::Pop)))
			((d-AddNoteToChord) (begin
					(doublestroke::showhelp #t) 					
					(Bind wrap:Op1 first)
					(Bind wrap:Op2 second)
					(Bind wrap:Op3 third)
					(Bind wrap:Op4 fourth)
					(Bind wrap:Op5 fifth)
					(Bind wrap:Op6 sixth)
					(Bind wrap:Op7 seventh)
					(Bind wrap:Op8 eighth)
					(Bind wrap:Op9 ninth)
					(Bind wrap:Op0 tenth)
					))
			(else (begin (Help::Pop) #f)))		  
		  (set! DenemoKeypressActivatedCommand #f))		  
		 (doublestroke::invokegui))) ; if not DenemoKeypressActivated

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
    (d-DirectivePut-header-override tag (logior DENEMO_OVERRIDE_TAGEDIT DENEMO_OVERRIDE_GRAPHIC))
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
  (d-DirectivePut-scoreheader-override tag (logior DENEMO_OVERRIDE_TAGEDIT DENEMO_OVERRIDE_GRAPHIC))
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

(define (CreateButton tag label)
  (d-DirectivePut-score-override tag DENEMO_OVERRIDE_GRAPHIC)
  (d-DirectivePut-score-display tag label))
  
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

;;;;;;;;; String Escaper
;;;;;;;;; Escapes Strings
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


#! (define latex-escape (string-escaper '((#\\ . "\\\\")
				       (#\~ . "\\~")
				       (#\# . "\\#")
				       (#\$ . "\\$")
				       (#\% . "\\%")
				       (#\^ . "\\^")
				       (#\& . "\\&")
				       (#\{ . "\\{")
				       (#\} . "\\}")
				       (#\_ . "\\_")))) !#


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
;;;; Send any question to Nils Gey denemo@nilsgey.de
;;;; Currently its only used to create/overwrite a page with a new script.
;;;; It uses the User-Rights System so its very secure. Vandalism in only possible in the same degree as allowed on the website itself.
;;;; All API access is done via (d-HTTP). The C function behind it sends HTTP-POST data to the given Server/Website and returns the HTTP-header and MediaWiki Data. 
;;;; The basic steps are 1)Login with Username/PW given in Denemos Preferences and 2)Create a HTTP-Cookie .
;;;; After that allowed Manipulation is possible. Currently we create request an Edit-Token and create a new Page.

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
				(string-append "format=json&action=login&lgname=" (scheme-escape(d-GetUserName)) "&lgpassword=" (scheme-escape(d-GetPassword)))))

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
					(RaiseError))))	

		; Prepare request Edit-Token.
		; First send d-HTTP, then parse the token, then modify it to the right format.
		(define (GetEditToken name CookieStringReturn)
			(define (ReceiveRawToken)
				(d-HTTP 
				HTTPHostname
				HTTPSite
				CookieStringReturn
				(string-append "format=json&action=query&prop=info|revisions&intoken=edit&titles="name)))	
			
			;json gives you +\\ @ Tokens end, but you need only +\ which is %2B%5C in url-endcoded format. 
			(string-append (string-trim-both (string-trim-both (ParseJson (ReceiveRawToken) "edittoken" ) #\\) #\+) "%2B%5C"))	
		
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
	"))
				
				;Send the data to let the API generate a new site!		
				(d-HTTP
					HTTPHostname
					HTTPSite
					CookieStringReturn
					(string-append "action=edit&title=" name "&format=json&summary=" tooltip "&text=" (SiteString) "&token=" (GetEditToken name CookieStringReturn)))	
				
				;Show script in browser
				(d-Help (string-append "http://" HTTPHostname "/index.php/" name))				
		
			); End of GetLicenseAndBuildString
		
		
		;check if Login/Building the Cookie was correct
		(if (string-ci=? CookieStringReturn "CookieError")
			(display "\nAn error occured while performing the task. Thats why the result of your Upload-Command is: ")
			(GetLicenseAndBuildString))
		);;;; End of OverwritePage 
		
		;;;; The real action happens here.	This is the only place where (CookieString) is called so we have only one Login at all.
		(display (OverwritePage (CookieString))) ;show and execute

	) ; End Of (d-UploadRoutine)


;;; play a note a mid-volume 80
(define* (PlayNote pitch duration #:optional (volume " 80"))
 (d-OutputMidiBytes (string-append "0x9$ " pitch volume))
 (d-OneShotTimer duration (string-append "(d-OutputMidiBytes " "\"" "0x8$ " pitch " 0" "\"" ")" )))

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


(define (MeasureEmpty?) (None?))

(define  (ColumnEmpty?)
	(define return #f)
	(define measure (d-GetMeasure)) ; to make shure we stay in the same column all the time.
	(d-PushPosition)
	(if (not (MoveToColumnStart))
	  #f ; if we have unequal staff length in staff 1 stop immediatly, 
	  (let loop ()		
		(if (and (None?) (equal? measure (d-GetMeasure)))
			(begin
				(set! return #t)
				(if (d-MoveToStaffDown)
					(loop)))		
			(set! return #f))))
	(d-PopPosition)
	return)

(define* (GetPrevailingTimeSig #:optional (numberorstring #f) ) 
	(if numberorstring
		(string->number (d-InsertTimeSig "query=timesigname"))
		(d-InsertTimeSig "query=timesigname")))

;Get Measure Filling Status in Ticks
(define (GetMeasureTicks)
	(let script ((return 0))
	(d-PushPosition)
	(GoToMeasureEnd)
	(set! return (d-GetEndTick))

	  (d-PopPosition)
	  return))

(define (MeasureFillStatus)
(define MaxTicks (* 1536 (GetPrevailingTimeSig #t) )) ; How many ticks are in a 100% filled measure?
(define ActualTicks (GetMeasureTicks))
(cond 
 	((not ActualTicks) #f) ; empty
 	((< ActualTicks MaxTicks) #f) ; underful
 	((= MaxTicks ActualTicks) 1)  ; 100% filled
 	((< MaxTicks ActualTicks) 2) ; >100% filled
	(else  (display "strange!")))) ; ?
 	


(define (EmptyMeasure?)
  (not (d-GetEndTick)))

(define (UnderfullMeasure?)
  (or (EmptyMeasure?)
     (not (MeasureFillStatus))))

(define (FullDurationMeasure?)
  (and (not (UnderfullMeasure?))
       (= 1 (MeasureFillStatus))))

(define (OverfullMeasure?)
  (eq? 2 (MeasureFillStatus)))
  ;;(and (not (EmptyMeasure?)) 
 ;; (> (d-GetEndTick) (* 1536 (GetPrevailingTimeSig #t)))))

(define (MeasureComplete?) (or (FullDurationMeasure?) 
			       (d-Directive-standalone? "Anacrusis")
			       (d-Directive-standalone? "ShortMeasure")))
   		
;;;;;;;;;;;;;;;;;
(define (DenemoFirst)
  (begin
    (display "DenemoFirst")))

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

(define (DefaultDenemoPlay)
   (d-Play "(display \"Here endeth a scripted playback\")"))

(define (DenemoPlay)
  (DefaultDenemoPlay))

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
    (d-Play "(if Playback::Loop (DenemoLoop))")))

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
;;;;;;;; DenemoConvert;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (DenemoConvert)
(define MidiNoteStarts (make-vector 256 #f))

(defstruct Note name start duration)
(define Notes '())

(if (d-RewindRecordedMidi)
    (let loop ((note #f)(tick 0))
      (set! note (d-GetRecordedMidiNote))
      (if note
	  (begin
	    (set! tick (d-GetRecordedMidiOnTick))
	    (if (< tick 0)
		(let ((on (vector-ref MidiNoteStarts note)))
		  (if on
		      (begin 
			(set! Notes (cons (list (make-Note 'name note 'start on 'duration (- (- tick) on))) Notes))
			(vector-set! MidiNoteStarts note #f)	
			(loop note tick))
		      (format #t "An off with no On\n")))
		(let ((on (vector-ref MidiNoteStarts note)))
		  (if on
		      (format #t "An on when already on\n")
		      (begin
			(vector-set! MidiNoteStarts note tick)
			(loop note tick)
		)))))
	  (begin         ;;;;;; finished processing the notes
	    (if (> (length Notes) 0)
		(let ()
		    (define (add-note note)
		      (if (Note? note)
			  (begin
			  (eval-string (string-append "(d-InsertNoteInChord \"" (d-GetNoteForMidiKey (Note.name note)) "\")")))
			  (format #t "\tNo note to add note ~a ~a to\n" (Note.name note) (Note.duration note))))
		    
		    (define (insert-note name dur)
		      (let ((base (duration::GuessBaseNoteInTicks dur)))
(format #t "have ~a ~a \n" base dur)
			(if base
			    (begin
			      (if (> (- dur base) (- (* 2 base) dur))
				  (set! base (* base 2)))
			      (begin 
				;(format #t "Create note ~a ~a\n"  (d-GetNoteForMidiKey name)   (duration::ticks->denemo base))
				(eval-string (string-append  "(d-Insert" (duration::ticks->denemo base)")"))
				(d-PutNoteName (d-GetNoteForMidiKey name)))))))
		    
	    
		    (set! Notes (reverse Notes))
     
;;;;;; change the list of Notes into a list of chords
		    (let loop ((index 0))

;;;;;;;;;;; overlap decides if two notes should be a chord	 
		      (define (overlap n1 n2)
			(if (list? n1)
			    (set! n1 (car n1)))
			(< (abs (- (Note.start n1) (Note.start n2))) 50))
;;;;;;;;;;;;;;;;;; end of overlap
		      
					;(format #t "Number of notes ~a\n" index)	      
		      (let ((note1 (list-ref Notes index))
			    (note2 #f))
			(if (> (length Notes) (+ 1 index))
			    (begin
			      (set! note2 (list-ref Notes (+ 1 index)))
			      (if (overlap note1 (car note2))
				  (begin
				    (list-set! Notes index (cons (car note2) note1))
				    (set! Notes (delq note2 Notes)))
				  (begin
					;(list-set! Notes index (list note1))
				    (set! index (+ index 1))))
			      (loop index))))) ;;;;;;; end of changing Notes to list of chords


;;;loop through the chords, getting a good duration value, the duration from one to the next and inserting
		    (let loop ((index 0))
		      (if (> (length Notes) (+ 1 index))
			  (let ((chord1 (list-ref Notes index))
				(chord2 #f)
				(duration #f))
			    (if (> (length Notes) (+ 1 index))
			     (begin
			      (set! chord2 (list-ref Notes (+ 1 index)))
			      (set! duration (- (Note.start (car chord2)) (Note.start (car chord1))))
			      (format #t "With duration ~a\n" duration)
			      (insert-note (Note.name (car chord1)) duration)
			       (for-each  add-note (cdr chord1))
			       (set! index (+ index 1))
			       (loop index))
			     (insert-note (Note.name (car chord1)) (Note.duration (car chord1)))))))
	    
		    (format #t "End of processing\n"))))));;;;;if rewind succeeded
		(format #t "No notes found in recording\n")))


;;;;;;;;;;;;;;;;;;;;;;;;

(define (DenemoPrintAllHeaders)
  (let ((lily "printallheaders"))
    (if (d-CheckLilyVersion "2.11.59")
	(set! lily "print-all-headers"))
     (d-DirectivePut-paper-postfix "PrintAllHeaders" (string-append lily " = ##t\n"))))

;;;; GoToMeasureEnd: Move right until "appending" or "none" which is the Measure End
(define (GoToMeasureEnd)
  (let loop ()
    (if  (or (None?) (Appending?))
	#t
	(begin (d-MoveCursorRight) (loop)))))

;;;; GoToMeasureBeginning
(define (GoToMeasureBeginning)
  (if (d-MoveToMeasureLeft)
	(d-MoveToMeasureRight)  
	(d-MoveToBeginning)))

;;; Go to the first staff, same measure. Handle crossing unequal staff length.
(define (MoveToColumnStart)
	(define measure (d-GetMeasure)) ; to make shure we stay in the same column all the time.
	(RepeatUntilFail d-MoveToStaffUp)
	(d-GoToPosition #f #f measure #f))

; Paste by Nils Gey, 2011
;; Multistaff-Pasting always adds the complete part AFTER the current measure or fills any complete set of empty measures
;; Singlestaff-Pasting happens at the cursor position and will just paste whats in the clipboard
(define (DenemoPaste)
  (define (Paste)
	(define paste:multistaff? (d-GetClipObjType 1 0))
	(define paste:howmanystaffs
		(let loop ((n 0))
		(if (d-GetClipObjType (1+ n) 0)
		(loop (1+ n))
		(1+ n))))
	(define position:startmeasure (d-GetMeasure))
	(define position:startstaff (d-GetStaff))
	(define staff 0)
	(define count -1)		
	(define staffcountlist (list 0)) ; used for multistaff			
	(define (1+count!)
		(set! count (1+ count)))
	(define (1+staff!)
		(set! staff (1+ staff)))
	(define (MeasuresToPasteToEmpty?)
		(define position:measure (d-GetMeasure))	
		(not (any not (map (lambda (x) (ProbePosition None? #f (+ position:startstaff x) (1+ position:measure) 1)) staffcountlist))))
	(define (SplitMeasure!)
		(if paste:multistaff?
			(let ()
				(define position:measure (d-GetMeasure))			
				(define position:current (GetPosition))
				(for-each (lambda (x) 
					(if (d-GoToPosition #f (+ position:startstaff x) (1+ position:measure) 1)
						(d-SplitMeasure) 
						(begin 
							(d-GoToPosition #f (+ position:startstaff x) 1 1) 
							(d-MoveToEnd) (d-SplitMeasure)))) ; for staff ends
					 staffcountlist)
				(apply d-GoToPosition position:current)
				(d-MoveToMeasureRight)) ; all needed empty measures got created			
			(d-SplitMeasure))) ; singlestaff is simple split.
	(define (MeasureBreak!)
		(if  (or (> staff 0) (MeasuresToPasteToEmpty?)) ; only the first staff needs to check if the next measure is empty or not. In Multistaff the first paste-round created all necessary empty measures for all other staffs so its just straight-forward pasting of objects.
			(d-MoveToMeasureRight)
			(SplitMeasure!)))
	(define (paste! staff count)		
		(case (d-GetClipObjType staff count)
			;In order of occurence, to boost performance.
			((0) (d-PutClipObj staff count))	;note, rest, gracenote, chord
			((8) (MeasureBreak!) ) ; Measurebreak
			((15) (d-PutClipObj staff count))	;lilypond-directive
			((1) (d-PutClipObj staff count))	;tuplet open
			((2) (d-PutClipObj staff count))	;tuplet close
			((5) (d-PutClipObj staff count))	;keysignature
			((4) (d-PutClipObj staff count))	;timesignature
			((3) (d-PutClipObj staff count))	;clef						
			((7) (d-PutClipObj staff count))	;stem-directive
			((9) #f) ; staffbreak 			
			((#f) #f) ; No object left. Means "no clipboard", too.
			(else (begin (display "Error! Object to paste unknown or unsupported\n") #f))))
			
	;body
	(d-UnsetMark)
	(set! staffcountlist (iota paste:howmanystaffs))	
	(if paste:multistaff? ; check if the staff-length of all participating staffs is equal. If not append measures.
		(let ()
		(define position:current (1+ position:startmeasure))
		(d-PushPosition)
		(for-each (lambda (x) 
		 	(if (d-GoToPosition #f (+ position:startstaff x) position:current 1)
		 		#t
		 		(begin ; fill in measures up to nr. position:startmeasure
		 			(d-GoToPosition #f (+ position:startstaff x) 1 1)
		 			(d-MoveToEnd)
		 			(Repeat d-AppendMeasure (- position:current (d-GetMeasure)))))) staffcountlist) 
		(d-PopPosition)))	
	(if paste:multistaff?  ; check if the current measure in all needed staffs is empty. If not create an empty measure to start.
		(if (any not (map (lambda (x) (ProbePosition None? #f (+ position:startstaff x) position:startmeasure 1)) staffcountlist))
			(MeasureBreak!)
			(set! position:startmeasure (1- position:startmeasure)))) 
	;Do the first staff. It will stop on staffbreak or end of the clipboard.	
	(RepeatUntilFail (lambda () (1+count!) (paste! staff count)))
	(if paste:multistaff? 
		(let ()
			(define position:return (GetPosition))			
			(Repeat  ; repeat single-staff pasting for each staff > 0.	
				(lambda ()					
					(1+staff!)
					(set! count -1)
					(if (d-GoToPosition #f (+ staff position:startstaff) (1+ position:startmeasure) 1) ; if a staff down, go there. else abort.
						(RepeatUntilFail (lambda () (1+count!) (paste! staff count)))							
						"No staff left to paste to. But the beginning of the clipboard was pasted, which is probably what you wanted."))
				paste:howmanystaffs)
			(apply d-GoToPosition position:return))))	
  (if (d-GetClipObjType 0 0) (Paste)))
 
;;;; Shuffling Sequences
;;; http://mumble.net/~campbell/scheme/shuffle.scm
;;; This code is written by Taylor R. Campbell and placed in the Public
;;; Domain.  All warranties are disclaimed.
;;; This uses SRFIs 1 (list-lib) and 8 (receive).

;;;; Merge Shuffle
;;; Partition the list into two equal halves; shuffle the two halves,
;;; and then merge them by randomly choosing which half to select the
;;; next element from.

(define (Flip-coin) 
	(if (= 1 (random 2))
		#t
		#f))


(define (Merge-shuffle-list list)

  (define (merge a b)
    (cond ((not (pair? a)) b)
          ((not (pair? b)) a)
          (else
           (if (Flip-coin)
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

(define (Merge-shuffle-list! list)

  (define (merge! a b)
    (cond ((null-list? a)       b)
          ((null-list? b)       a)
          ((Flip-coin)          (%merge! a b) a)
          (else                 (%merge! b a) b)))

  (define (%merge! a b)
    (cond ((null-list? (cdr a))
           (set-cdr! a b))
          ((Flip-coin)
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
  (eval-string (lyimport::import))
  (d-MoveToMovementBeginning))

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
  (inexact->exact (string->number (number->string return))))

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
	(else #f)))

; Calculate a new Duration in Ticks with a basic note value and a number of augmentation-dots
(define (duration::CalculateTicksWithDots baseTicks numberOfDots)
	; x = basic note value
	; n = number of dots
	; 2x - x/2^n 
	(-  (* 2 baseTicks) (/ baseTicks (expt 2 numberOfDots))))

; Calculate how many dots a tick value has. Needs base duration, too.
(define (duration::CalculateDotsFromTicks ticks base)
; x = base , y = ticks. result is the number of dots
; log(x/(2*x-y))  / log(2)
 (define return (/  (log (/ base (- (* base 2) ticks)))   (log 2) ))
 (duration::inexact->exact return))

; Get Number of Dots from a Lilypond string like "2.". Its so basic it will work on Denemo notes, too.
(define (duration::GetNumberOfDotsInLilypond input)
 (length (cdr (string-split input #\.))))

; For the sake of completenes. Denemo and Lilypond dot-syntax is just the same, only the number itself is different.
(define (duration::GetNumberOfDotsInDenemo input)
	(duration::GetNumberOfDotsInLilypond input))

(define (duration::denemo->lilypond number)
	(define return (expt 2 number))
	return)

(define (duration::lilypond->denemo number)
	(define return (/ (log number) (log 2))	)
	 (duration::inexact->exact return))

(define (duration::denemo->ticks number) ; increases with negative integers
	(define return (* (expt 2 (- 8 number)) 6))
	return)

(define (duration::lilypond->ticks number) ; increases with 0.5, 0.25 etc.
	(define return (* (expt 2 (- 8 (/ (log number) (log 2)))) 6))
	 (duration::inexact->exact return))

; Ticks->Denemo wants a number but returns a string because of dots
(define* (duration::ticks->denemo number #:optional (basenumber number))
 (define numberOfDots  (duration::CalculateDotsFromTicks number basenumber))
;n = -(log(y/3)-9*log(2))/log(2) 
 (define return (- (/ (- (log (/ basenumber 3)) (* 9 (log 2))) (log 2))))
 (set! return (duration::inexact->exact return))
 (string-append (number->string return) (string-concatenate (make-list numberOfDots "."))))

;Ticks->Lilypond wants a number but returns a string because of dots
(define* (duration::ticks->lilypond number #:optional (basenumber number))
 ;Equation in readable form: http://www.numberempire.com/equationsolver.php?function=y+%3D+6*2^%288-n%29&var=n&answers=
 (define numberOfDots  (duration::CalculateDotsFromTicks number basenumber))
 (define return  (expt 2 (- (/ (- (log (/ basenumber 3)) (* 9 (log 2))) (log 2)))))
 (set! return (duration::inexact->exact return))
 (string-append (number->string return) (string-concatenate (make-list numberOfDots "."))))

;SplitTicksToBaseDurations wants a single tick value as number and splits it to the lowest count of base durations
;;Returns a list of base durations
;;TODO: if the last step is no BaseDuration return it anyway and an additional #f to signal the problem. Most likely someone tried to involve an incomplete tuplet.
;;TODO: Basic version. Duration will be correct but maybe ugly.
(define (duration::SplitTicksToBaseDurations ticks)
(let loop ((number ticks) (returnlist (list #f)))
	(define basedur (duration::GuessBaseNoteInTicks number))	
	(if basedur
		(loop (- number basedur) (append returnlist (list basedur)))
		(if (= number 0)
			(list-tail returnlist 1)
			(list-tail (append returnlist (list number #f)) 1)))))			 

;;;;;;;;;; End of duration-conversion

;; Applied duration scripts
(define (duration::GetNumberOfDotsInTicks) ; Fails for tuplets
	 (duration::CalculateDotsFromTicks (d-GetDurationInTicks) (duration::GetBaseDurationInTicks)))
	   

(define (duration::GetBaseDurationInTicks)
	(define ticks (d-GetBaseDurationInTicks))
	(if ticks
		(abs ticks)
		#f))	   

(define (duration::GetSelectionDurationInTicks)
	(if (d-MarkStatus)
		(apply + (MapToSelection d-GetDurationInTicks Music?))
		#f))
    

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
   (else   #f ))) 
; Second step: add dots
  ; d-ChangeN work on appending position, but Breve and Longa not. But d-AddDot works on appending, too. So we must rule Appending out, else it will add dots without changing the duration for breve and longa.
  (if (and (Music?) (integer? ticks) (integer? dots) (changeBase ticks)) ; <-- the action changeBase itself needs to be a test, too. 
  (let loop ((i 0))
	(if (= dots i)
	#t
	(begin
	   (d-AddDot)  
	   (loop (+ i 1)))))
  #f))

(define (duration::InsertBaseDurationList basedurlist ansNotes)
	(define itemnumber (length basedurlist))
	(let loop ((counter 0))
		(if (and (> counter 0) (= (list-ref basedurlist counter) (/ (list-ref basedurlist (1- counter)) 2)))  ; current duration is half of previous one = current is a dot. First position excluded.
			(begin (d-MoveCursorLeft) (d-AddDot) (d-MoveCursorRight))
			(begin (ANS::InsertNotes ansNotes) (d-MoveCursorLeft) (duration::ChangeNoteDurationInTicks (list-ref basedurlist counter)) (d-ToggleTie)  (d-MoveCursorRight)))		
		(if (>= counter (1- itemnumber))
			(begin (d-MoveCursorLeft) (d-ToggleTie) (d-MoveCursorRight) #t) ; last item was already inserted. Undo the last tie. The End.
			(begin (loop (1+ counter))))))	

;; Meter related functions
(define (duration::GetNumerator) (string->number (car (string-split  (GetPrevailingTimeSig) #\/))))
(define (duration::GetDenominator) (string->number (cadr (string-split  (GetPrevailingTimeSig) #\/))))
(define (duration::GetDenominatorInTicks) (* 1536 (/ 1 (duration::GetDenominator))))
(define (duration::GetWholeMeasureInTicks) (* 1536 (GetPrevailingTimeSig #t)))
(define (duration::GetMetricalPosition) (1+ (/ (d-GetStartTick) (duration::GetDenominatorInTicks))))
(define (duration::MetricalMain? position) (integer? position))


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
(defstruct musobj pitch movement staff measure metricalp horizontal start duration baseduration dots)

; Actually create the music-object. In this process various information are collected.
;;(define testob (CreateMusObj))  (set!musobj.duration testob 256)  (display (musobj.start testob))
(define (CreateMusObj) 
	(make-musobj 'pitch (ANS::GetChordNotes)
				 'movement (d-GetMovement)
				 'staff (d-GetStaff)
				 'measure (d-GetMeasure)
				 'horizontal (d-GetHorizontalPosition)
				 'metricalp (duration::GetMetricalPosition)
				 'start (d-GetStartTick)
				 'duration (d-GetDurationInTicks)
				 'baseduration (d-GetBaseDurationInTicks)
				 'dots (d-GetDots)				 
				 ))					


(define (DefaultInitializePrint) (display "\nstarting to print\n"))
(define (DefaultFinalizePrint) (display "\nfinished print\n"))

(define (DefaultInitializePlayback) (display "\nstarting to playback\n"))
(define (DefaultFinalizePlayback) (display "\nfinished playback\n"))

(define (DefaultInitializeMidiGeneration) (display "\nstarting to generate MIDI\n"))
(define (DefaultFinalizeMidiGeneration) (display "\nfinished MIDI generation\n"))

(define (DefaultInitializeTypesetting) (display "\nstarting to generate LilyPond\n"))
(define (DefaultFinalizeTypesetting) (display "\nfinished generating LilyPond\n"))


(define (InitializePrint) (DefaultInitializePrint))
(define (FinalizePrint) (DefaultFinalizePrint))

(define (InitializePlayback) (DefaultInitializePlayback))
(define (FinalizePlayback) (DefaultFinalizePlayback))

(define (InitializeMidiGeneration) (DefaultInitializeMidiGeneration))
(define (FinalizeMidiGeneration) (DefaultFinalizeMidiGeneration))

(define (InitializeTypesetting) (DefaultInitializeTypesetting))
(define (FinalizeTypesetting) (DefaultFinalizeTypesetting))

;;;;;;Apply the passed script to each movement of a score
(define (ForAllMovements script)
  (d-PushPosition)
  (d-GoToPosition 1 1 1 1)
  (let loop ()
    (begin
      (eval-string script)
      (if (d-NextMovement)
	  (loop))))
  (d-PopPosition))
  
;Aliases for Breve and Longa to use with Denemo numbers for durations.
(define (d-3072) (d-Breve))
(define (d-6411) (d-Longa))
(define (d--3072) (d-Breve))
(define (d--6411) (d-Longa))
(define (d-Insert3072) (d-InsertBreve))
(define (d-Insert6411) (d-InsertLonga))
(define (d-Insert-3072) (d-InsertBreve))
(define (d-Insert-6411) (d-InsertLonga))
(define (d-Set3072) (d-SetBreve))
(define (d-Set6411) (d-SetLonga))
(define (d-Set-3072) (d-SetBreve))
(define (d-Set-6411) (d-SetLonga))
(define (d-Change3072) (d-ChangeBreve))
(define (d-Change6411) (d-ChangeLonga))
(define (d-Change-3072) (d-ChangeBreve))
(define (d-Change-6411) (d-ChangeLonga))

;Insert a no-pitch note of the prevailing duration.
(define (d-Enter) (eval-string (string-append "(d-" (number->string (abs (d-GetPrevailingDuration))) ")" )))

(define (GetPosition)
	(list (d-GetMovement) (d-GetStaff) (d-GetMeasure)(d-GetHorizontalPosition)))

;Radiobox is a Radio-Box list where you have a pretty name and a data type.
;;Wants pairs, car is a string to show as radio-option, cdr is a return value and can be any data type, for example a function.
(define (RadioBoxMenu . parameters)
	(define answer #f)
	(define radiostring (string-join (map (lambda (x) (car x)) parameters) stop)) 
	(set! answer (d-GetOption radiostring))
	(if answer
		(cdr	(list-ref  parameters (list-index (lambda (x) (equal?  answer (car x))) parameters)))
		#f))

(define (Probe test moveinstruction)
	(define return #f)
	(d-PushPosition)	
	(if (moveinstruction)
		(set! return (test)))
	(d-PopPosition)
	return)
(define (ProbePosition test movement staff measure horizontalposition)
	 (Probe test (lambda () (d-GoToPosition movement staff measure horizontalposition))))
(define (ProbePreviousMeasure test)
	(Probe test d-MoveToMeasureLeft))
(define (ProbeNextMeasure test)
	(Probe test d-MoveToMeasureRight))
(define (ProbeNextObject test)
	(Probe test d-NextObject))
(define (ProbePreviousObject test)
	(Probe test d-PreviousObject))
(define (ProbeNextNote test)
	(Probe test d-NextNote))
(define (ProbePreviousNote test)
	(Probe test d-PreviousNote))
	
;Protofunction for all transpose and shift related commands
;; Get all notes on cursor position and create a list with new values which then exchanges the current notes on cursor position
(define (ShiftProto method)
	(if (Note?) 
		(ANS::ChangeChordNotes (map method (ANS::GetChordNotes)))
		#f)) ; not a note/chord	

;Give the name of the Lilypond note on the current vertical cursor position.
;;It doesn't matter if an object is present or not.
(define (GetCursorNoteAsLilypond)
	(define midioctave (+ -4 (quotient (d-GetCursorNoteAsMidi) 12)))
	(define basenote (d-GetCursorNote))
	(define octavemod "")
	(set! octavemod 
		(if (negative? midioctave)
			(make-string (abs midioctave)  #\,)	
			(make-string midioctave  #\')))	
	(string-append basenote octavemod))


;Remember the users choice of an interval in this global var
(define GlobalRememberInterval "p5")

;A user dialog to ask for an interval. 
;;User has two different options: insert an interval like m2 or p5 directly or give two notes.
;;Return value is a number for the ANS pillar of 5th.
(define (AskForInterval)
	(define interval (d-GetUserInput "Please enter a transpose interval" "Please enter a transpose interval or two notes in Lilypond syntax.\n\nExample: m2 minor second, M2 major second, p5 fifth, T tritone.\nOr:  c' e' for a major third.\nFor a complete list please read the manual." GlobalRememberInterval))
	(set! GlobalRememberInterval interval)
	(if (ANS::IntervalGetSteps (string->symbol interval))
		(ANS::IntervalGetSteps (string->symbol interval))
		(let ()
			(define listy (map (lambda (x) (ANS::Ly2Ans (string->symbol x))) (string-tokenize interval)))
			(car (apply ANS::GetIntervall listy)))))

(define (ChangeToRest)
;TODO: (d-RemoveNoteFromChord) always returns #f so we have to use (d-GetNotes) as test until this gets fixed
	(if (Music?)
		(RepeatProcWhileTest d-RemoveNoteFromChord d-GetNotes)
		#f))
		
; A copy variant in Scheme
;; Save the selection in a scheme variable
;; Music? are musobj (CreateMusObj)
;;TODO: SchemeCopy and Paste are very limited and need improvement.
(define (SchemeCopy)
  ;If on an end-tuplet marker it gives you the startvalue
  (define (GetTupletFromEndTuplet)
		(let ()
		(define return #f)
			(if (equal? (d-GetType) "TUPCLOSE")
			(begin
				(d-PushPosition)
				(let loop ()
				(if (d-MoveCursorLeft)
					(if (equal? (d-GetType) "TUPOPEN")
						(set! return (d-GetTuplet))
						(loop))
					#f)) ; staff beginning		
				(d-PopPosition)))
				return))  
  ;Mainfunction to gather data.
  (define (gather)
  	(ActionChooser 
		(lambda () (CreateMusObj)) ;chords, notes, rests
		(lambda () (cons 'TUPCLOSE (GetTupletFromEndTuplet))) ; tuplet close
		(lambda () (cons 'TUPOPEN (d-GetTuplet))) ; tuplet open
		(lambda () (disp "lily")) ; lilypond directive
		(lambda () (disp "clef")) ; clefs
		(lambda () (disp "time")) ; timesignatures
		(lambda () (disp "key")) ; keysignatures
		(lambda () (disp "stem")))) ; stem directives /voice presets		
  (if (d-MarkStatus)
	(MapToSelection gather)		
	#f))
	
(define (ProcessSchemeCopyBufferMusObj musobjproc copybuffer)
	;modify the current musobj and then return the complete, altered, object for the map-list.
	(map (lambda (current) 
		(if (musobj? current) 
			(begin (musobjproc current) current) ; if museobj use the proc
			current)) ; if not just return the original object
		 copybuffer))
	
;Paste a list created by (SchemeCopy)
(define (SchemePaste listy)
  (define (insert x)
  	(cond
  	((musobj? x)  (ANS::InsertNotes (musobj.pitch x) (musobj.baseduration x) (musobj.dots x)))
  	((equal? (car x) 'TUPCLOSE) (d-EndTuplet))
  	((equal? (car x) 'TUPOPEN) (begin (d-StartTriplet) (d-SetTuplet (cdr x))))
  	
  	))
  	(for-each (lambda (x) (insert x)) listy))

  	
;ActionChooser is a meta function to provide a simple way to react to all types of Denemo items in the score.
(define (ActionChooser chord tupclose tupopen lilydirective clef timesig keysig stemdirective)	
	(define type (string->symbol (d-GetType)))
	(case type
	 	((CHORD) (chord))
	 	((TUPCLOSE) (tupclose))
	 	((TUPOPEN)  (tupopen))
	 	((LILYDIRECTIVE) (lilydirective))
	 	((CLEF) (clef))	
	 	((TIMESIG) (timesig))
	 	((KEYSIG) (keysig))
	 	((STEMDIRECTIVE) (stemdirective))
	 	(else #f)))

			#! (define (ActionChooserExample)
				  (ActionChooser 
					(lambda () (disp "Chord")) ;chords, notes, rests
					(lambda () (disp "tupclose")) ; tuplet close
					(lambda () (disp "Tupopen")) ; tuplet open
					(lambda () (disp "lily")) ; lilypond directive
					(lambda () (disp "clef")) ; clefs
					(lambda () (disp "time")) ; timesignatures
					(lambda () (disp "key")) ; keysignatures
					(lambda () (disp "stem")))) ; stem directives /voice presets
				(MapToSelection ActionChooserExample)!#
