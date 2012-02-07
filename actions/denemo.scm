(use-modules (srfi srfi-1)) ; List library
(use-modules (srfi srfi-8)) ; Returning and Accepting Multiple Values
(use-modules (srfi srfi-13)) ; String library
(use-modules (ice-9 regex)) ; regular expressions
(use-modules (ice-9 optargs)) ; optional (define* ) arguments
(use-modules (ice-9 q)) ; queue module

(define (use-denemo string)
	;(use-modules (actions denemo-modules (string->symbol string)))) ; maybe not (string->symbol) but (eval)
	(load (string-append "denemo-modules/" string ".scm")))

;Load additional Denemo functions. These are technically in the same global namespace as functions defined directly in denemo.scm. 
(use-denemo "scheme") ; Standalone functions and general Scheme helpers, not tied to any Denemo C-functions or Scheme functions which are not in the file itself.
(use-denemo "ans") ; Abstract Note System for pitch calculations
(use-denemo "notationmagick") ; Insert and modify, mostly randomized, music. Depends on ans.scm
(use-denemo "abstractionmovement") ; Create an abstract form of the music in Scheme for further analysing. Depends on ans.scm 
(use-denemo "commandlist")  ; Provide scrolling up and down through a list of commands. An extended toggle through multiple states.
(use-denemo "helpsystem") ; An online help system to display text in the second status bar
(use-denemo "selection")  ; Selections, Copy and Paste
(use-denemo "rhythmandmeter") ; Rhythm, Durations, Ticks, Meter, Conversion between Lilypond, Tick and Denemo duration.
(use-denemo "directives") ; Functions to handle the built-in Denemo directives.
(use-denemo "types") ; Denemo type related functions and tests. ("CHORD", "DIRECTIVE" etc.)
(use-denemo "moveandsearch") ; Move the cursor to all kinds of positions, loop through the score to find things.
;(use-denemo "deprecated") ; Old and outdated scripts
(use-denemo "fonts") ; define font utf-8 value

;Denenmo.scm is for functions that 
;; directly influence the Denemo GUI
;; or the keybindings
;; work with Denemo controls
;; are not part of a set currently (Creating a whole new file for just one function will not improve anything)
;; leftovers from the "one big denemo.scm file" time :)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Needed to see if lyimport / mxml import is called from inside or outside Denemo
(define Denemo #t)

(define DenemoKeypressActivatedCommand #f) ;;;is true while a keyboard shortcut is invoking a script, unless the script has set it to #f

(define (lyimport::load-file pathname filename)
  (load (string-append DENEMO_ACTIONS_DIR "lyimport.scm"))
  (set! lyimport::pathname pathname) 
  (set! lyimport::filename filename)
  (eval-string (lyimport::import))
  (d-MoveToMovementBeginning))

; GetUniquePairs is a function that takes a list and combines each value with any other, but without duplicates and in order.
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
		
; GetUniquePairsFilterLowest is a GetUniquePairs variant that sorts the list ascending and returns only those items which begin with the lowest value. In musical terms only pitches with the bass-note.
;;; (a b c d) -> ab ac ad			
(define* (GetUniquePairsFilterLowest listy #:optional (minimum min))
	(define lowest (apply minimum listy))
	(define returnList (GetUniquePairs listy)) ; sort the list ascending, so the lowest note/bass-note is the first.
	;(filter (lambda (x) (if (equal? (car x) lowest) #t #f)) returnList) ; return only those pairs which has lowest value as car
	(filter (lambda (x) (if (or (equal? (car x) lowest) (equal? (cdr x) lowest)) #t #f))  returnList)) ; return only those pairs which has the lowest value as car or cdr.

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

(define MusicalSymbols-notes (vector Denemo-Note0 Denemo-Note1  Denemo-Note2 Denemo-Note3 Denemo-Note4 Denemo-Note5 Denemo-Note6 Denemo-Note7 Denemo-Note8))
(define MusicalSymbols-sharp "\xe2\x99\xaf") ;;;may need to specify Denemo font for windows
(define MusicalSymbols-flat "\xe2\x99\xad")

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
				(set! gui-version (apply RadioBoxMenu (delete (cons "" False) 
					(list first second third fourth fifth sixth seventh eighth ninth tenth (cons "Lock in" 
					(lambda ()
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
						(Bind wrap:Op0 tenth)))))))
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
					(Help::RemoveTag 'doublestroketemp) ; Remove help message from temp system.
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

; ExtraOffset
;; the parameter "what" is the LilyPond grob that is being tweaked - it may not be the tag of the DenemoDirective that is being edited
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
    
; SetRelativeFontSize
(define* (SetRelativeFontSize what #:optional (type "chord") (context ""))
  (SetValue ChangeRelativeFontSize " #'font-size = #" what type context))

; SetPadding
(define* (SetPadding what  #:optional (type "chord") (context ""))
  (SetValue ChangePad " #'padding = #" what type context))

; SetValue
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

; ChangeOffset
;; e.g.  (define prefixstring      "\\once \\override Fingering  #'extra-offset = #'(")
;; (define postfix ")")

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
    (if (not title)
      (begin
	(set! title (d-GetUserInput (string-append type " " fieldname)
				    (string-append "Give a name for the " fieldname " of the " type) current))
	(if title
	  (begin
	    (if (string-null? title)
	      (d-DirectiveDelete-header tag)
	      (begin
		(set! title (scheme-escape title ))
		(d-DirectivePut-header-override tag (logior DENEMO_OVERRIDE_TAGEDIT DENEMO_OVERRIDE_GRAPHIC))
		(d-DirectivePut-header-display tag (string-append type " " fieldname ": " (html-escape title)))
		(d-DirectivePut-header-postfix tag (string-append field " = \"" title "\"\n")))))
	    (disp "Cancelled\n"))))))

; SetScoreHeaderField sets a field in the score header
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

(define (CreateButton tag label)
  (d-DirectivePut-score-override tag (logior DENEMO_OVERRIDE_MARKUP DENEMO_OVERRIDE_GRAPHIC))
  (d-DirectivePut-score-display tag label))


;;; play a note a mid-volume 80
(define* (PlayNote pitch duration #:optional (volume " 80"))
 (d-OutputMidiBytes (string-append "0x9$ " pitch " " volume))
 (d-OneShotTimer duration (string-append "(d-OutputMidiBytes " "\"" "0x8$ " pitch " 0" "\"" ")" )))

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


; DenemoConvert
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
     
(define* (SetQuarterCommaMeanTone #:optional (thestep 0))
  (let ((C     "67 ")
	(C#    "43 ")
	(Db    "84 ")
	(D     "60 ")
	(D#    "36 ")
	(Eb    "78 ")
	(E     "53 ")
	(E#    "29 ")
	(F     "71 ")
	(F#    "46 ")
	(Gb    "88 ")
	(G     "64 ")
	(G#    "40 ")
	(Ab    "81 ")
	(A     "57 ")
	(A#    "32 ")
	(Bb    "74 ")
	(B     "50 ")
	(Cb    "91 ")
	(B#     "26 "))
	  
	(cond ((= thestep 0) 
					;Eb-G#
		  (d-OutputMidiBytes (string-append "0xf0 0x7f 0x7f 0x08 0x08   0x03 0x7f 0x7f " C C# D Eb E F F# G G# A Bb B " 0xf7")))
		 ((= thestep 1) 
					;D#-Bb
		  (d-OutputMidiBytes (string-append "0xf0 0x7f 0x7f 0x08 0x08   0x03 0x7f 0x7f " C C# D D# E F F# G G# A Bb B " 0xf7")))	
		 ((= thestep 2) 
		 ;A#-F
		  (d-OutputMidiBytes (string-append "0xf0 0x7f 0x7f 0x08 0x08   0x03 0x7f 0x7f " C C# D D# E F F# G G# A A# B " 0xf7")))
		 ((= thestep 3) 
					;E#-C
		  (d-OutputMidiBytes (string-append "0xf0 0x7f 0x7f 0x08 0x08   0x03 0x7f 0x7f " C C# D D# E E# F# G G# A A# B " 0xf7")))
		 ((= thestep 4) 
					;B#-G
		  (d-OutputMidiBytes (string-append "0xf0 0x7f 0x7f 0x08 0x08   0x03 0x7f 0x7f " B# C# D D# E E# F# G G# A A# B " 0xf7")))
		 ((= thestep -1) 
					;Ab-C#
		  (d-OutputMidiBytes (string-append "0xf0 0x7f 0x7f 0x08 0x08   0x03 0x7f 0x7f " C C# D Eb E F F# G Ab A Bb B " 0xf7")))
		 ((= thestep -2) 
					;Db-F#
		  (d-OutputMidiBytes (string-append "0xf0 0x7f 0x7f 0x08 0x08   0x03 0x7f 0x7f " C Db D Eb E F F# G Ab A Bb B " 0xf7")))
		 ((= thestep -3) 
					;Gb-B
		  (d-OutputMidiBytes (string-append "0xf0 0x7f 0x7f 0x08 0x08   0x03 0x7f 0x7f " C Db D Eb E F Gb G Ab A Bb B " 0xf7")))
		 ((= thestep -4) 
					;Cb-E
		  (d-OutputMidiBytes (string-append "0xf0 0x7f 0x7f 0x08 0x08   0x03 0x7f 0x7f " C Db D Eb E F Gb G Ab A Bb Cb " 0xf7"))))))
   
(define MIDI-shortcuts::alist '(("" . "")))
(define (SetMidiShortcut shortcut command)
	(set! MIDI-shortcuts::alist (assoc-set! MIDI-shortcuts::alist shortcut command)))

(SetMidiShortcut "FootpedalUp" #f);;;set these to a command name, e.g. InsertA for the action (d-InsertA) 
(SetMidiShortcut "FootpedalDown" #f)

(define (MIDI-shortcut::controller type value)
;;(format #t "controller type ~a value ~a\n" type value)
  (cond ((and (equal? type 64) (equal? value 127))
    	  (assoc-ref MIDI-shortcuts::alist "FootpedalDown"))
        ((and (equal? type 64) (equal? value 0))
    	  (assoc-ref MIDI-shortcuts::alist "FootpedalUp"))

        ((equal? type 1)
	 (let ((thestep  (round(/ (- value 64) 16))))
	   (PlayNote  
	    (number->string  (+ 60 (* 4 thestep) ))
	    100)
	 (SetQuarterCommaMeanTone thestep)
	 (d-SetEnharmonicPosition thestep)
	 (d-RefreshDisplay)
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
                      
; Create a music-object that holds various information. This is the smallest, single object 
(defstruct musobj pitch movement staff measure horizontal metricalp start duration baseduration dots)

; Actually create the music-object. In this process various information are collected.
;;(define testob (CreateMusObj))  (set!musobj.duration testob 256)  (display (musobj.start testob))
(define (CreateMusObj)
  (if (MeasureEmpty?)
	; Measure emtpy, create whole measure rest musobj
	(make-musobj 'pitch (list +inf.0)
				 'movement (d-GetMovement)
				 'staff (d-GetStaff)
				 'measure (d-GetMeasure)
				 'horizontal (d-GetHorizontalPosition)
				 'metricalp 1
				 'start 0
				 'duration (duration::GetWholeMeasureInTicks)
				 'baseduration (duration::GetWholeMeasureInTicks)
				 'dots 0		 
				 )	
	; Measure not emtpy
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
				 )))					

(define (CreateMusObjCursorNote)		
		(define note(GetNoteUnderCursorAsLilypond))
		(define return (CreateMusObj))
		(if note
			(begin	(set!musobj.pitch return (list (ANS::Ly2Ans (string->symbol note))))
			return)
			#f))

;Find the lowest pitch from the parameters, which are musobj.			
(define (MusObj::minPitch . objects)
	 (reduce 
	 	(lambda (x y)
	 		(if (< (car (musobj.pitch x))  (car (musobj.pitch y)))
	 			 x
	 			 y))
	 	#f
	 	objects))

;Find the highest pitch from the parameters, which are musobj.	 	
(define (MusObj::maxPitch . objects)
	 (reduce 
	 	(lambda (x y)
	 		(if (> (car (musobj.pitch x))  (car (musobj.pitch y)))
	 			 x
	 			 y))
	 	#f
	 	objects))
	 		 	
;Converts two MusObjs to an interval numbe (ans syntax. steps in the pillar of 5th)
(define (MusObj::GetInterval one two)
	(ANS::GetInterval (car (musobj.pitch one)) (car (musobj.pitch two)))) 			

(define (MusbObj::MoveTo musobj)
	(d-GoToPosition (musobj.movement musobj) (musobj.staff musobj) (musobj.measure musobj) (musobj.horizontal musobj)))		

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


;Radiobox is a Radio-Box list where you have a pretty name and a data type.
;;Wants pairs, car is a string to show as radio-option, cdr is a return value and can be any data type, for example a function.
(define (RadioBoxMenu . parameters)
	(define answer #f)
	(define radiostring (string-join (map (lambda (x) (car x)) parameters) stop)) 
	(set! answer (d-GetOption radiostring))
	(if answer
		(cdr	(list-ref  parameters (list-index (lambda (x) (equal?  answer (car x))) parameters)))
		#f))

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
	
(define (GetNoteUnderCursorAsLilypond)
	(if (Note?)
		(let ()
			(define current (GetCursorNoteAsLilypond))
			(if (any (lambda (x) (equal? x current)) (string-tokenize (d-GetNotes)))
			 current
			 #f))
		 #f))
 
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
			(apply ANS::GetInterval listy))))

(define (ChangeToRest)
;TODO: (d-RemoveNoteFromChord) always returns #f so we have to use (d-GetNotes) as test until this gets fixed
	(if (Music?)
		(RepeatProcWhileTest d-RemoveNoteFromChord d-GetNotes)
		#f))
		
;Insert an object that just takes space and ticks in the denemo display and playback. No lilypond meaning.
;;It emulates the note-entry behaviour: A step right afterwards and creates new measures if necessary or continues in empty measures.
;;This is the basis for things like multi measure rests, longa, breve and other "Scheme Music Objects".
(define* (InsertNullObject ticks #:optional (tag (number->string (random 1000))))	
	(if (and (Appending?) (MeasureFillStatus)) ; is the current measure already full and are we in the appending position? if not just insert the object.
		(let ()
			(define next (ProbeNextMeasure d-GetType)) 
			(cond 
				((equal? #f next) (d-SplitMeasure))
				((string=? next "None") (d-MoveToMeasureRight))))) ; next measure is empty
	(d-DirectivePut-standalone tag)
	(d-DirectivePut-standalone-minpixels tag (/ ticks 12))
	(d-SetDurationInTicks ticks)
	(d-MoveCursorRight))


;;; Insert a directive that puts a comment in the LilyPond text showing where the source material for this part of the music is
;;; The tag name is a command to open the file containing that source
(define (InsertLink filepos)
  (d-Directive-standalone  "DenemoLink")
  (d-DirectivePut-standalone-postfix "DenemoLink" (string-append "%{" filepos "%}"))
  (d-DirectivePut-standalone-minpixels "DenemoLink" 30)
  (d-DirectivePut-standalone-graphic "DenemoLink" "\n⬏\n\n36")
  (d-MoveCursorRight)
  (d-RefreshDisplay))
  

;;; The routine called by the DenemoLink command to follow the link
(define (FollowLink)
  (let ((link (d-DirectiveGet-standalone-postfix "DenemoLink")))
    (if link
     (begin
     ;(disp "link is " link "ok\n")
      (set! link (string-trim-both link   (lambda (c)(or (eqv? c #\{) (eqv? c #\%)))))
  (d-OpenSource link)))))
