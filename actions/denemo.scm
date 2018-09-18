(use-modules (srfi srfi-1)) ; List library
(use-modules (srfi srfi-8)) ; Returning and Accepting Multiple Values
(use-modules (srfi srfi-13)) ; String library
(use-modules (ice-9 regex)) ; regular expressions
(use-modules (ice-9 optargs)) ; optional (define* ) arguments
(use-modules (ice-9 q)) ; queue module

;;; for guile 2.0 compatibility define the define-once procedure to work in guile 1.8
(cond-expand
   (guile-2) ; nothing
   (else ; guile < 2.0
    (define-macro (define-once sym exp)
      `(define ,sym
         (if (module-locally-bound? (current-module) ',sym)
             ,sym
             ,exp)))))
             
(define (use-denemo string)
    (load-from-path (string-append string ".scm")))


;(bindtextdomain "denemo" "/usr/local/share/locale") find prefix!!!
(define (_ msg) (gettext msg "denemo"))

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
(use-denemo "wysiwyg") ; procedures for performing wysiwyg operations on the print view window
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

(define DENEMO_WEIGHT_NORMAL "0");CAIRO_FONT_WEIGHT_NORMAL
(define DENEMO_WEIGHT_BOLD "1");CAIRO_FONT_WEIGHT_BOLD
(define DENEMO_SLANT_NORMAL "0");CAIRO_FONT_SLANT_NORMAL
(define DENEMO_SLANT_ITALIC "1");CAIRO_FONT_SLANT_ITALIC
(define DENEMO_SLANT_OBLIQUE "2");CAIRO_FONT_SLANT_OBLIQUE


 
(define DenemoWholeMeasureRestTag "WholeMeasureRest") ;several commands have to coordinate there behavior around whole measure rests, which are not built-in
(define DenemoWholeMeasureRestCommand d-WholeMeasureRest)
(define DenemoWholeMeasureRestParams 'WholeMeasureRest::params)  ;these three must match for this to work.

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
;; Returns #f if not a chord or appending
(define (GetLowestNote)
    (if (Note?)
     (list-ref (string-tokenize(d-GetNotes)) 0 )
     #f))

(define (GetHighestNote)
    (if (Note?)
     (list-ref (reverse (string-tokenize(d-GetNotes))) 0)
     #f))

; Get highest and lowest note at or before cursor as lilypond syntax. Works on Chords and Single Notes and in appending position.
;; Returns #f if not a chord, when appending reports chord before cursor
(define (GetLowestAvailableNote)
    (define notes (d-GetNotes))
    (if notes 
     (list-ref (string-tokenize notes) 0 )
     #f))
(define (GetHighestAvailableNote)
    (define notes (d-GetNotes))
    (if notes 
     (list-ref ((reverse string-tokenize notes)) 0 )
     #f))


(define MusicalSymbols-notes (vector Denemo-Note0 Denemo-Note1  Denemo-Note2 Denemo-Note3 Denemo-Note4 Denemo-Note5 Denemo-Note6 Denemo-Note7 Denemo-Note8))
(define MusicalSymbols-sharp "\xe2\x99\xaf") ;;;may need to specify Denemo font for windows
(define MusicalSymbols-flat "\xe2\x99\xad")

(define cue-Advanced (_ "Advanced"))
(define cue-PlaceAbove (_ "Place above staff"))
(define cue-PlaceBelow (_ "Place below staff"))
(define cue-SetRelativeFontSize (_ "Set Relative Font Size"))
(define cue-OffsetPositionAll (_ "Offset Position (All)"))
(define cue-OffsetPositionOne (_ "Offset Position (One)"))
(define cue-EditText (_ "Edit Text"))
(define cue-SetPadding (_ "Set Padding"))
(define cue-Delete (_ "Delete"))
(define cue-Edit (_ "Edit"))
(define cue-RestorePosition (_ "Restore Position")) 
(define cue-NudgePosition (_ "Nudge Position")) 

;(define cue- "")

(define (GetNudge)
    (let ((offsetx "0")(offsety "0"))
            (set! offsetx (d-GetUserInput (_ "Offset Position") (_ "Amount (+/-) to nudge in horizontal direction") offsetx))
            (if offsetx
                (begin
                    (set! offsety (d-GetUserInput (_ "Offset Position") (_ "Amount (+/-) to nudge in vertical direction") offsetx))
                    (if offsety
                        (cons offsetx offsety)
                        #f))
                #f)))

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
             (begin ;FallBack ; create a gui from the given parameters, test for cancel-button #f
                (set! gui-version (apply RadioBoxMenu (delete (cons "" False) 
                    (list first second third fourth fifth sixth seventh eighth ninth tenth 

            ))))
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
    
         (doublestroke::invokegui)) ;  DenemoKeypressActivated has been dropped as it is not working

(define (DenemoAbbreviatedString title)
    (html-escape (if (< (string-length title) 14) title (string-append (substring title 0 10) "..."))))

;;;;;;;;;; SetHeaderField sets a field in the movement header
;;;;;;;;;; the directive created is tagged Score or Movement depending on the field

(define* (SetHeaderField field #:optional (title #f) (escape #t) (movement #f)(extra-space "0")(bold #f)(italic #f)(fontsize #f))
  (let ((current "") (thematch #f) (tag "") (type "") (fieldname "")(data #f))
    (if (or (equal? field "subtitle") (equal? field "subsubtitle") (equal? field "piece"))
     (begin
       (set! type "Movement")
       (if (equal? field "subtitle")
                    (set! fieldname "Title"))
       (if (equal? field "subsubtitle")
                    (set! fieldname "Subtitle"))
             (if (equal? field "piece")
                    (set! fieldname "Piece")))
     (begin
       (set! type "Score")
       (set! fieldname (string-capitalize field))))
     (if movement
        (set! type "Movement"))
    (set! tag (string-append type fieldname)) 
    
    
    (set! current (d-DirectiveGet-header-data tag))
    ;;;old versions have a string, new versions an alist beginning 'right paren ie ' 0x28 in Unicode 50 octal.
    (if (and current (> (string-length current)) (eq? (string-ref current 0) #\') (eq? (string-ref current 1) #\50))
        (begin
            (set! data (eval-string current))
            (set! extra-space (assq-ref data 'extra-space))
            (set! bold (assq-ref data 'bold))
            (set! fontsize (assq-ref data 'fontsize))
            (set! current (assq-ref data 'title)))
        (set! data '()))
    
    (if (not current)
        (set! current (d-DirectiveGet-header-display tag)))
    (if (not current)
            (set! current ""))      
                    
    (if (not title)
            (set! title (d-GetUserInput (string-append type " " fieldname)
                    (string-append "Give a name for the " fieldname " of the " type) current #t)))
  (if (and title fontsize)
    (begin
       (set! bold (RadioBoxMenu (cons (_ "Bold") " ") (cons (_ "Normal") " \\normal-text ")))
       (if (not bold) (set! bold ""))
       (set! italic (RadioBoxMenu  (cons (_ "Upright") "") (cons (_ "Italic") " \\italic ")))
       (if (not italic) (set! italic ""))
       (set! extra-space (d-GetUserInput (string-append "Score " field) 
                        (_ "Extra space above (0):") extra-space #t))
       (if not extra-space (set! extra-space "0"))

       (set! fontsize (d-GetUserInput (_ "Font Magnification") (_ "Give font magnification required (+/-) 0 ") fontsize))
       (if (not fontsize)
            (set! fontsize "0")))
    (begin
        (set! bold "")
        (set! italic "")
        (set! fontsize "0")))
              
    (if title
      (begin
                (d-SetSaved #f)
                (if (string-null? title)
                    (d-DirectiveDelete-header tag)
                    (let ((movement (number->string (d-GetMovement))))
                        (set! data (assq-set! data 'title title))
                        (set! data (assq-set! data 'bold bold))
                        (set! data (assq-set! data 'italic italic))
                        (set! data (assq-set! data 'fontsize fontsize))
                        (set! data (assq-set! data 'extra-space extra-space))
                        (if escape (set! title (scheme-escape title )))
                        (d-DirectivePut-header-override tag (logior DENEMO_OVERRIDE_TAGEDIT DENEMO_OVERRIDE_GRAPHIC))
                        (d-DirectivePut-header-data tag (format #f "'~s" data))
                        
                        (d-DirectivePut-header-display tag (DenemoAbbreviatedString title))
                        (d-DirectivePut-header-postfix tag (string-append field " = \\markup {\\vspace #'" extra-space " \\fontsize #'" fontsize italic bold  " \\with-url #'\"scheme:(d-GoToPosition " movement " 1 1 1)(d-" type fieldname ")\" "  "\"" title "\"}\n")))))
        (disp "Cancelled\n"))))

; SetScoreHeaderField sets a field in the score header
(define* (SetScoreHeaderField field  #:optional (title #f) (escape #t) (full-title #f) (extra-space "0")(bold #f)(italic #f)(fontsize #f))
(let ((current "") (tag "")(data #f))
  (set! tag (string-append "Score" (string-capitalize field)))
  (set! current (d-DirectiveGet-scoreheader-data tag))
      ;;;old versions have a string, new versions an alist beginning 'right paren ie ' 0x28 in Unicode 50 octal.
  (if (and current (> (string-length current)) (eq? (string-ref current 0) #\') (eq? (string-ref current 1) #\50))
        (begin
            (set! data (eval-string current))
            (set! extra-space (assq-ref data 'extra-space))
            (set! bold (assq-ref data 'bold))
            (set! fontsize (assq-ref data 'fontsize))
            (set! current (assq-ref data 'title)))
        (set! data '()))
        
  (if (not current)
        (set! current (d-DirectiveGet-scoreheader-display tag)))
  (if (not current)
      (set! current ""))
  (if (not title)
      (set! title  (d-GetUserInput (string-append "Score " field) 
                  (_ "Give a name applying to the whole score") current #t)))
 
    ;;;if we have a pre-existing title ask about details, if the user hasn't cancelled the title
  (if (and title fontsize)
    (begin
       (set! bold (RadioBoxMenu (cons (_ "Bold") " ") (cons (_ "Normal") " \\normal-text ")))
       (if (not bold) (set! bold ""))
       (set! italic (RadioBoxMenu(cons (_ "Upright") "") (cons (_ "Italic") " \\italic ")))
       (if (not italic) (set! italic ""))
       (set! extra-space (d-GetUserInput (string-append "Score " field) 
                        (_ "Extra space above (0):") extra-space #t))
       (if not extra-space (set! extra-space "0"))
       (set! fontsize (d-GetUserInput (_ "Font Magnification") (_ "Give font magnification required (+/-) 0 ") fontsize))
       (if (not fontsize)
            (set! fontsize "0")))
    (begin
        (set! bold "")
        (set! italic "")
        (set! fontsize "0")))
     
  (if title
    (begin
      (d-SetSaved #f)      
            (if escape (set! title (scheme-escape title )))
            (if (string-null? title)
                    (d-DirectivePut-scoreheader-override tag 0)
                    (begin
                            (set! data (assq-set! data 'title title))
                            (set! data (assq-set! data 'bold bold))
                            (set! data (assq-set! data 'italic italic))
                            (set! data (assq-set! data 'fontsize fontsize))
                            (set! data (assq-set! data 'extra-space extra-space))

                            (d-DirectivePut-scoreheader-override tag (logior DENEMO_OVERRIDE_TAGEDIT DENEMO_OVERRIDE_GRAPHIC))
                            (d-DirectivePut-scoreheader-data tag (format #f "'~s" data))
                            (d-DirectivePut-scoreheader-display tag (DenemoAbbreviatedString title))))
            (if (not full-title)
                (set! full-title (string-append " \\markup {\\vspace #'" extra-space " \\fontsize #'" fontsize italic bold " \\with-url #'\"scheme:(d-" tag ")\"  "  "\"" title "\"}\n")))
            (d-DirectivePut-scoreheader-postfix tag (string-append field " = " full-title "\n"))))))

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

(define DenemoPutMidi d-PutMidi) ;;hook for intercepting MIDI filter output
    
(define (DenemoSetPlaybackStart)
    (d-Stop)
    (d-RecreateTimebase)
    (let ((start (d-GetMidiOnTime)))
        (if (not start)
            (begin
                (d-PushPosition)
                (while (and (eqv? 0 (d-GetDurationInTicks))
                        (d-NextObject)))
                (set! start (d-GetMidiOnTime))
                (d-PopPosition)))
        (if start
            (begin
                (d-SetPlaybackInterval start #t)
                (d-RefreshDisplay)))
        start))

(define (DenemoSetPlaybackEnd)
  (d-Stop)
  (d-RecreateTimebase)
  (let ((stop (d-GetMidiOffTime)))
    (if (not stop)
        (begin
            ;;(if (not (Appending?))
            (d-PushPosition)
            (while (and (d-PrevObject)
                            (zero? (d-GetDurationInTicks))))   ;)
            (while (and (eqv? 0 (d-GetDurationInTicks)) (d-NextObject)))
            (set! stop (d-GetMidiOffTime))
            (d-PopPosition)))
    (if (and stop (> stop 0))
        (begin
            (d-SetPlaybackInterval #t stop)
            (d-RefreshDisplay)))
    stop))
    
(define (DenemoSetPlaybackIntervalToSelection)
  (begin
    (d-Stop)
    (let ((start #f)(end #f))
      (set! end (d-GetMidiOffTime))
      (if (boolean? end)
      (d-RecreateTimebase))
      (set! end (d-GetMidiOffTime))
      (if (boolean? end)
      (d-WarningDialog (_ "End the selection at a note"))
      (begin
        (d-GoToMark)
        (set! start (d-GetMidiOnTime))
        (if (boolean? start)
        (d-WarningDialog (_ "Start the selection at a note"))
        (begin
          (if (< end start)
              (d-SetPlaybackInterval end start)
              (d-SetPlaybackInterval start end))
          (d-RefreshDisplay))))))))

;;;
(define (CurrentMeasureOnTime)
  (define ontime #f)
  (d-PushPosition)
  (while (d-MoveToStaffUp))
  (let staffloop ()
    (while (d-PrevObjectInMeasure))
    (let loop ((this (d-GetMidiOnTime)))
      (if this
    (if ontime
      (if (< this ontime)
        (set! ontime this))
      (set! ontime this))
    (if (d-NextObjectInMeasure)
      (loop (d-GetMidiOnTime)))))
    (if (d-MoveToStaffDown)
    (staffloop)))
  (d-PopPosition)
  ontime)
  
(define (CurrentMeasureOffTime)
  (define offtime #f)
  (d-PushPosition)
  (while (d-MoveToStaffUp))
  (let staffloop ()
  (while (d-NextObjectInMeasure))
  (let loop ((this (d-GetMidiOffTime)))
    (if this
      (if offtime
    (if (> this offtime)
      (set! offtime this))
      (set! offtime this))
    (if (d-PrevObjectInMeasure)
      (loop (d-GetMidiOffTime)))))
    (if (d-MoveToStaffDown)
    (staffloop)))
  (d-PopPosition)
  offtime)
  
(define DenemoClickTrack (_ "Click Track")) 

;;;
(define d-GetOnsetTime d-GetMidiOnTime)  ;;;was a duplicate, not used by Denemo
; DenemoConvert
(define (DenemoConvert)
    (define MidiNoteStarts (make-vector 256 #f))
    (defstruct Note name start duration)
    (define Notes '())
    (if (d-RewindRecordedMidi)
        (let loop ((note #f)(tick 0))
          (set! note (d-GetRecordedMidiNote)) ;(disp "note is " note "\n")
          (if note
              (begin
                (set! tick (d-GetRecordedMidiOnTick)) ;(disp "tick is " tick "\n")
                (if (< tick 0) ;;; note OFF
                    (let ((on (vector-ref MidiNoteStarts note)))
                      (if on
                          (begin 
                            (set! Notes (cons (list (make-Note 'name note 'start on 'duration (- (- tick) on))) Notes))
                            (vector-set! MidiNoteStarts note #f)    
                            (loop note tick))
                          (disp "An off with no On\n")))
                    (let ((on (vector-ref MidiNoteStarts note))) ;;;note ON
                      (if on
                          (disp "An on when already on\n")
                          (begin
                            (vector-set! MidiNoteStarts note tick)
                            (loop note tick))))))
              (begin         ;;;;;; finished processing the notes
                (if (> (length Notes) 0)
                        (let ()
                            (define (add-note note)
                              (if (Note? note)
                                  (begin
                                    (eval-string (string-append "(d-InsertNoteInChord \"" (d-GetNoteForMidiKey (Note.name note)) "\")")))
                                  (disp "\tNo note to add note ~a ~a to\n" (Note.name note) (Note.duration note))))
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
                              (let ((note1 (list-ref Notes index))(note2 #f))
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
(define (DenemoHasBookTitles)
 (d-LilyPondInclude (cons 'query "book-titling.ily"))
    (if (not LilyPondInclude::return)
        (d-LilyPondInclude (cons 'query "simplified-book-titling.ily")))
  LilyPondInclude::return)
(define (DenemoUseBookTitles)
         (d-LilyPondInclude "simplified-book-titling.ily"))
(define (DenemoHideBookTitles)
    (d-LilyPondInclude (cons 'delete "book-titling.ily"))
    (d-LilyPondInclude (cons 'delete "simplified-book-titling.ily")))
(define (DenemoPrintAllHeaders)
  (if (DenemoHasBookTitles)
    (begin
      (d-WarningDialog (_ "You had book titles for this score. These are being dropped. To re-instate them, re-set the title as a book title."))
      (DenemoHideBookTitles)))
  (d-DirectivePut-paper-postfix "PrintAllHeaders" "\nprint-all-headers = ##t\n"))
     
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

(define Pitchbend::commandUp #f)
(define Pitchbend::commandDown #f)
(define  Pitchbend::timer 0)
(define (MIDI-shortcut::pitchbend value)
  ;(format #t "pitch bend value ~a\n" value)
  (cond ((> value 64)   
     (if  Pitchbend::commandUp
      (eval-string Pitchbend::commandUp)))
         ((< value 64)  
      (if  Pitchbend::commandDown
      (eval-string Pitchbend::commandDown)))))
                      
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
            (begin  (set!musobj.pitch return (list (ANS::Ly2Ans (string->symbol note))))
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

(define (DefaultInitializePrint) (d-Info "Starting to print"))
(define (DefaultFinalizePrint) (d-Info "Finished print"))

(define (DefaultInitializePlayback) (d-Info "Starting to playback"))
(define (DefaultFinalizePlayback) (d-Info "Finished playback"))

(define (DefaultInitializeMidiGeneration) (d-Info "Starting to generate MIDI"))
(define (DefaultFinalizeMidiGeneration) (d-Info "Finished MIDI generation"))

(define (DefaultInitializeTypesetting) (d-Info "Starting to generate LilyPond"))
(define (DefaultFinalizeTypesetting) (d-Info "Finished generating LilyPond"))


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


;RadioBoxMenu is a Radio-Box list where you have a pretty name and a data type.
;;takes any number of pairs as parameters, car is a string to show as radio-option, cdr is a return value and can be any data type, for example a function.
;;or any number of strings - returns the string chosen or #f
;;RadioBoxList takes a list of the parameter types as above, returns chosen value or #f        
(define (RadioBoxMenuList  parameters)
    (define answer #f)
    (define radiostring #f)
    (if (string? (car parameters))
        (set! radiostring (string-append (string-join parameters stop) stop))
        (set! radiostring (string-append (string-join (map (lambda (x) (car x)) parameters) stop) stop)))
    (set! answer (d-GetOption radiostring))
    (if answer
        (if (string? (car parameters))
             answer
             (cdr (list-ref  parameters (list-index (lambda (x) (equal?  answer (car x))) parameters))))
        #f))        
(define (RadioBoxMenu . parameters)
    (RadioBoxMenuList parameters))

(define (TitledRadioBoxMenuList title parameters)
   (define answer #f)
    (define radiostring #f)
    (if (string? (car parameters))
        (set! radiostring (string-append (string-join parameters stop) stop))
        (set! radiostring (string-append (string-join (map (lambda (x) (car x)) parameters) stop) stop)))
    (set! answer (d-GetOption radiostring title))
    (if answer
        (if (string? (car parameters))
             answer
             (cdr (list-ref  parameters (list-index (lambda (x) (equal?  answer (car x))) parameters))))
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
    (define interval (d-GetUserInput (_"Please enter a transpose interval") (_ "Please enter a transpose interval or two notes in Lilypond syntax.\n\nExample: m2 minor second, M2 major second, p5 fifth, T tritone.\nOr:  c' e' for a major third.") GlobalRememberInterval))
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
  (d-DirectivePut-standalone-data "DenemoLink" filepos)
  (d-DirectivePut-standalone-minpixels "DenemoLink" 30)
  (d-DirectivePut-standalone-graphic "DenemoLink" "\n⬁\ndenemo\n24")
  (d-DirectivePut-standalone-gy "DenemoLink" 10)
  (d-MoveCursorRight)
  (d-RefreshDisplay))
  

;;; The routine called by the DenemoLink command to follow the link
;(define (FollowLink)
;  (let ((link (d-DirectiveGet-standalone-postfix "DenemoLink")))
;    (if link
;     (begin
     ;(disp "link is " link "ok\n")
;      (set! link (string-trim-both link   (lambda (c)(or (eqv? c #\{) (eqv? c #\%)))))
;  (d-OpenSource link)))))

;;;; Routines for audio annotation
(define (DenemoAudioAnnotation timing)
  (define (set-tempo)
    (if (d-MeasureLeft)
      (let ((last-timing (string->number (d-DirectiveGet-standalone-display "Timing"))))
    (if last-timing
      (let ((diff (- timing last-timing)))
        (set! diff (floor (* (/ (/ (duration::GetWholeMeasureInTicks) 384) diff) 60)))
        (d-DirectivePut-standalone-override "Timing" (logior DENEMO_OVERRIDE_TEMPO DENEMO_OVERRIDE_STEP))
        (d-DirectivePut-standalone-graphic "Timing" (string-append "\n𝅘𝅥 = " (number->string diff) " \ndenemo\n24"))
        (d-DirectivePut-standalone-gy  "Timing" -40)
        (d-DirectivePut-standalone-display "Timing" "")
        (d-DirectivePut-standalone-midibytes "Timing" (number->string diff))))
    (d-MeasureRight))))
        
    (disp "timing " timing "\n")
    (if timing
    (begin
      (set-tempo)
      (d-DirectivePut-standalone-display "Timing" (number->string timing))
      (if (zero? (GetMeasureTicks))
    (d-WholeMeasureRest))
      (if (not (d-MeasureRight))
    (begin
      (d-AppendMeasureAllStaffs)
      (d-MeasureRight))))))


(define (DenemoAudioAnnotate)
    (DenemoAudioAnnotation  (d-NextAudioTiming)))
;;;; Updates a standalone directive at the cursor if it is marked as DYNAMIC. Updating is by running the tag with parameter 'recalculate
(define (UpdateStandaloneDirective)
   (define tag (d-DirectiveGetTag-standalone))
    (if tag 
        (let ((override
        (d-DirectiveGet-standalone-override tag)))
        (if (not (zero? (logand override DENEMO_OVERRIDE_DYNAMIC)))
            (eval-string (string-append "(d-" tag " 'recalculate)"))))))
;;;;;;
(define (GetEditOption)
    (define  choice (d-GetOption  (string-append cue-Edit stop  cue-Delete stop cue-Advanced stop)))
   (cond
     ((boolean? choice)
      'cancel)
     ((equal? choice  cue-Advanced)
      'advanced)
     ((equal? choice cue-Delete)
      'delete)
     ((equal? choice cue-Edit)
      'edit)))

;;;;
(define (MidiInput?) (= (d-GetInputSource) DENEMO_INPUTMIDI))
(define (AudioInput?) (= (d-GetInputSource) DENEMO_INPUTAUDIO))
(define (KeyboardInput?) (= (d-GetInputSource) DENEMO_INPUTKEYBOARD))

;;;;;
(define* (StandaloneText tag text #:optional (direction "-") (italic "\\italic ") (bold "\\bold "))
    ;;;(set! tag (string-append tag "\n" text)) not needed, and with accented characters can cause illegal chars sent to pango_layout_set_text
    (d-Directive-standalone tag)
    (d-DirectivePut-standalone-prefix tag "<>")
    (d-DirectivePut-standalone-postfix tag (string-append direction "\\markup " italic bold "{\"" (scheme-escape text) "\" }"))
    (d-DirectivePut-standalone-grob tag "Text")
    (d-DirectivePut-standalone-display tag text)
    (d-DirectivePut-standalone-minpixels tag 30))
;;;;;;;;    
(define (GetLilyPondDirection)
    (RadioBoxMenu (cons (_ "Up") "^") (cons (_ "Down") "_") (cons (_ "Auto") "-")))
;;;;
(define (CheckForLilyPondDefine name)
    (if (d-Directive-score? (string-append "Allow\n" name))
        #t
        (let ((filename
            (string-append DENEMO_LOCAL_ACTIONS_DIR "//graphics//" name ".eps")))
            (if (d-FileExists filename)
                    (begin
                        (d-WarningDialog (_ "This definition is not loaded ... trying to load"))
                        (d-CustomOrnamentDefinition (list name filename 2))
                        )
                    #f))))
                
                
;;;;;;;;;;
(define (AllowOrnament name)
    (let ((filename (scheme-escape (string-append DENEMO_GRAPHICS_DIR name ".eps"))))
        (d-LilyPondDefinition (cons name (string-append "\\markup {\\epsfile #X #2 #\"" filename "\""   "}" )))
        (d-DirectivePut-score-data (string-append "Allow\n" name) (string-append "(list \"" name "\" \"" filename  "\" \"2\")"))))
(define (ToggleOrnament name params)
    (let ((tag (string-append "Toggle" (string-upcase name 0  1))))
        (ChordOrnament tag (string-append "\\" name)   params   name)
    (if (not (d-Directive-score? (string-append "Allow\n" name)))
        (AllowOrnament name))))
   
 
;;;;;;;;;
(define (GetDefinitionDirectives)
    (define directives '())
    (let loop ((count 0))
        (define good-tag (d-DirectiveGetNthTag-score count))
        (if good-tag 
            (begin
                (if (string-prefix? "Allow\n" good-tag)
                    (set! directives (cons good-tag directives)))
                (loop (1+ count)))))
    directives)

(define (GetDefinitionDataFromUser)
    (let  ((directives '())(definitions #f))
        (define (get-second-line text)
            (let ((thelist (string-split text #\newline)))
                (if (> (length thelist) 1)
                (list-ref thelist 1)
                "")))
        (define (extract-data tag)
            (define name (get-second-line tag))
            (cons name (d-DirectiveGet-score-data tag)))
        (set! directives (GetDefinitionDirectives))

        (if (not (null? directives))
            (set! definitions (map extract-data directives)))
        (if definitions
                (RadioBoxMenuList definitions)
                #f)))

;;; get the (lilypond internal) step and accidental for a lilypond syntax note name
(define (GetStep note)
    (let ((step #f))
        (case (string-ref note 0)
            ((#\c) (set! step 0))
            ((#\d) (set! step 1))
            ((#\e) (set! step 2))
            ((#\f) (set! step 3))
            ((#\g) (set! step 4))
            ((#\a) (set! step 5))
            ((#\b) (set! step 6)))
       step))
(define (GetAccidental note)
    (define NATURAL 0)
    (define FLAT (/ -1 2))
    (define SHARP (/ 1 2))
    (define DOUBLE-FLAT -1)
    (define DOUBLE-SHARP 1)
    (if (string-contains note "isis")
        DOUBLE-SHARP
        (if (string-contains note "eses")
            DOUBLE-FLAT
            (if (string-contains note "is")
                SHARP
                (if (string-contains note "es")
                    FLAT
                    NATURAL)))))
;;;;;;;
(define (DenemoDefaultBeatStructure beats)
  (define bs "")
  (if (zero? (remainder beats 3))
    (begin
        (set! beats (number->string (/ beats 3)))
        (set! bs (string-append beats " " beats " " beats)))
    (if (zero? (remainder beats 2))
        (begin
            (set! beats (number->string (/ beats 2)))
            (set! bs (string-append beats " " beats)))
        (set! bs (string-append (number->string (/ (- beats 1) 2)) " " (number->string (- beats (/ (- beats 1) 2)))))))
    bs)

;;;
(define (DenemoGetDuration title)
    (TitledRadioBoxMenuList title
        (list (cons "𝅝" "1/1")
        (cons "𝅗𝅥" "1/2")
        (cons "𝅘𝅥" "1/4")
        (cons "𝅘𝅥𝅮" "1/8") 
        (cons "𝅘𝅥𝅯" "1/16")
        (cons "𝅘𝅥𝅱" "1/32")
        (cons "𝅘𝅥𝅱" "1/64")
        (cons "𝅘𝅥𝅱" "1/128"))))
                            
;;
(define (DenemoSetTitles tag param editing)
    (let ((score (equal? tag "ScoreTitles"))
        (data #f)
        (dedication #f)
        (title #f)
        (subtitle #f)
        (subsubtitle #f)
        (instrument #f)
        (poet #f)
        (composer #f)
        (meter #f)
        (arranger #f)
        (tagline #f)
        (copyright #f)
        (piece #f)
        (opus #f))
        (define (get-field field initial)
            (if (not initial)
                (set! initial field))
            (if editing
                (let ((response  (d-GetUserInputWithSnippets  (if score (_ "Score Titles")  (_ "Movement Titles")) (string-append (_ "Give ") field) initial)))
                    (if response
                        (car response)
                        #f))
                (let ((response (d-GetUserInput (if score (_ "Score Titles")  (_ "Movement Titles")) (string-append (_ "Give ") field) initial)))
                    (if response (lilypond-markup-escape response) #f))))
            
        (define (write-titles)
            (let ((header ""))
                (define (url type)
                    (if score
                            (string-append "\\with-url #'\"scheme:(DenemoSetTitles \\\"" tag "\\\" '" type " #t)\" ")
                            (string-append "\\with-url #'\"scheme:(d-GoToPosition " (number->string (d-GetMovement)) " 1 1 1)(DenemoSetTitles \\\"" tag "\\\" '" type " #t)\"")))
            
                (if dedication
                                (set! header (string-append header "     dedication = \\markup " (url "dedication") " {" dedication "}\n"))
                                (set! header (string-append header "     dedication = ##f\n")))
                (if title
                                (set! header (string-append header "     title = \\markup " (url "title") " {" title "}\n"))
                                (set! header (string-append header "     title = ##f\n")))
                (if subtitle
                                (set! header (string-append header "     subtitle = \\markup " (url "subtitle") " {" subtitle "}\n"))
                                (set! header (string-append header "     subtitle = ##f\n")))
                (if subsubtitle
                                (set! header (string-append header "     subsubtitle = \\markup " (url "subsubtitle") " {" subsubtitle "}\n"))
                                (set! header (string-append header "     subsubtitle = ##f\n")))
                (if instrument
                                (set! header (string-append header "     instrument = \\markup " (url "instrument") " {" instrument "}\n"))
                                (set! header (string-append header "     instrument = ##f\n")))
                (if poet
                                (set! header (string-append header "     poet = \\markup " (url "poet") " {" poet "}\n"))
                                (set! header (string-append header "     poet = ##f\n")))
                (if composer
                                (set! header (string-append header "     composer = \\markup " (url "composer") " {" composer "}\n"))
                                (set! header (string-append header "     composer = ##f\n")))
                (if meter
                                (set! header (string-append header "     meter = \\markup " (url "meter") " {" meter "}\n"))
                                (set! header (string-append header "     meter = ##f\n")))
                (if arranger
                                (set! header (string-append header "     arranger = \\markup " (url "arranger") " {" arranger "}\n"))
                                (set! header (string-append header "     arranger = ##f\n")))
                (if tagline
                                (set! header (string-append header "     tagline = \\markup " (url "tagline") " {" tagline "}\n"))
                                (set! header (string-append header "     tagline = ##f\n")))
                (if copyright
                                (set! header (string-append header "     copyright = \\markup " (url "copyright") " {" copyright "}\n"))
                                (set! header (string-append header "     copyright = ##f\n")))
                (if piece
                                (set! header (string-append header "     piece = \\markup " (url "piece") " {" piece "}\n"))
                                (set! header (string-append header "     piece = ##f\n")))
                (if opus
                                (set! header (string-append header "     opus = \\markup " (url "opus") " {" opus "}\n"))
                                (set! header (string-append header "     opus = ##f\n")))
                (d-SetSaved #f)
                
                (DenemoPrintAllHeaders);;; here to disable book titles.
                (if (not (d-Directive-header? "MovementTitles"))
                 (d-DirectiveDelete-paper "PrintAllHeaders"))
             
                (if score
                    (begin
                        (d-DirectivePut-scoreheader-postfix tag header)
                        (d-DirectivePut-scoreheader-display tag (_ "Score Titles"))
                        (d-DirectivePut-scoreheader-override tag DENEMO_OVERRIDE_GRAPHIC))
                    (begin
                        (d-DirectivePut-header-postfix tag header)
                        (d-DirectivePut-header-display tag (_ "Movement Titles"))
                        (d-DirectivePut-header-override tag DENEMO_OVERRIDE_GRAPHIC)))
                ;;; if setting movement titles but no score titles are set then initialize score titles to #f
                (if (and (not score) (not (d-Directive-scoreheader? "ScoreTitles")))
                        (DenemoSetTitles "ScoreTitles" 'initialize #f))))
                        
(define (form-pair name title)
    (string-append "(cons '" name (if (string? title) (string-append " \"" (scheme-escape title) "\"") " #f") ")"))
;;; procedure starts here
        (if score
            (set! data (d-DirectiveGet-scoreheader-data tag))
            (set! data (d-DirectiveGet-header-data tag)))
        (if data 
            (begin
                (set! data (eval-string data))
                (set! dedication (assq-ref data 'dedication))
                (set! title (assq-ref data 'title))
                (set! subtitle (assq-ref data 'subtitle))
                (set! subsubtitle (assq-ref data 'subsubtitle))
                (set! instrument (assq-ref data 'instrument))
                (set! poet (assq-ref data 'poet))
                (set! composer (assq-ref data 'composer))
                (set! meter (assq-ref data 'meter))
                (set! arranger (assq-ref data 'arranger))
                (set! tagline (assq-ref data 'tagline))
                (set! copyright (assq-ref data 'copyright))
                (set! piece (assq-ref data 'piece))
                (set! opus (assq-ref data 'opus))))
        (if (not data)
            (set! data '()))

        (let ((choice (list 
                (cons (_ "dedication") 'dedication)
                (cons (_ "title") 'title)
                (cons (_ "subtitle") 'subtitle)
                (cons (_ "subsubtitle") 'subsubtitle)
                (cons (_ "instrument") 'instrument)
                (cons (_ "poet") 'poet)
                (cons (_ "composer") 'composer)
                (cons (_ "meter") 'meter)
                (cons (_ "arranger") 'arranger))))
                
                
                
            (if score
                (set! choice (append choice (list (cons (_ "tagline") 'tagline) (cons (_ "copyright") 'copyright))))
                (set! choice (append choice (list (cons (_ "piece") 'piece) (cons (_ "opus") 'opus)))))

           (if (d-Directive-scoreheader? "BookTitle")
                (let ((decide (RadioBoxMenu (cons (_ "Switch to Simple Titles") 'switch) (cons (_ "Cancel") #f))))
                    (if decide
                        (begin
                            (d-DirectiveDelete-score "TopMargin")
                            (d-LilyPondInclude (cons 'delete "simplified-book-titling.ily"))
                            (set! arranger (d-DirectiveGet-scoreheader-data "BookArranger"))
                            (set! composer (d-DirectiveGet-scoreheader-data "BookComposer"))
                            (set! instrument (d-DirectiveGet-scoreheader-data "BookInstrumentation"))
                            (set! copyright (d-DirectiveGet-scoreheader-data "BookCopyright"))
                            (set! meter (d-DirectiveGet-scoreheader-data "BookDate"))
                            (set! poet (d-DirectiveGet-scoreheader-data "BookPoet"))
                            (set! title (d-DirectiveGet-scoreheader-data "BookTitle"))
                            (d-DirectiveDelete-scoreheader "BookArranger")
                            (d-DirectiveDelete-scoreheader "BookComposer")
                            (d-DirectiveDelete-scoreheader "BookInstrumentation")
                            (d-DirectiveDelete-scoreheader "BookCopyright")
                            (d-DirectiveDelete-scoreheader "BookDate")
                            (d-DirectiveDelete-scoreheader "BookPoet")
                            (d-DirectiveDelete-scoreheader "BookTitle"))
                        (set! param 'abort))))
                
            (if param
                (set! choice param)
                (set! choice (RadioBoxMenuList choice)))
            (if (null? data)
                (begin
                    (if (or (d-DirectiveGet-scoreheader-postfix "ScoreTitle")
                            (d-DirectiveGet-header-postfix "ScoreSubsubtitle")
                            (d-DirectiveGet-header-postfix "ScoreSubtitle")
                            (d-DirectiveGet-header-postfix "ScoreArranger")
                            (d-DirectiveGet-header-postfix "ScoreComposer")
                            (d-DirectiveGet-header-postfix "ScoreDedication")
                            (d-DirectiveGet-scoreheader-postfix "ScoreInstrument")
                            (d-DirectiveGet-header-postfix "ScorePoet")
                            (d-DirectiveGet-header-postfix "ScorePiece")
                            (d-DirectiveGet-header-postfix "ScoreOpus")
                            (d-DirectiveGet-header-postfix "ScoreMeter")
                            (d-DirectiveGet-header-postfix "ScoreTagline")
                            (d-DirectiveGet-scoreheader-postfix "ScoreCopyright")
                            (d-DirectiveGet-header-postfix "MovementTitle")
                            (d-DirectiveGet-header-postfix "MovementSubtitle")
                            (d-DirectiveGet-header-postfix "MovementPiece"))
                        (begin
                            (set! choice 'abort)
                            (d-WarningDialog (_ "You have simple titles created by an earlier version of Denemo.\nYou can only edit these with 1.2.4 or earlier versions.\nYou can delete them in the score and movement editor and then re-instate them."))))))
                
 
            (case choice
                ((dedication)
                    (if dedication (set! editing #t))
                    (set! choice (get-field (_ "dedication") dedication))
                    (if choice (set! dedication choice)))
                ((title)
                    (if title (set! editing #t))
                    (set! choice (get-field (_ "title") title))
                    (if choice (set! title choice)))
                ((subtitle)
                    (if subtitle (set! editing #t))
                    (set! choice (get-field (_ "subtitle") subtitle))
                    (if choice (set! subtitle choice)))
                ((subsubtitle)
                    (if subsubtitle (set! editing #t))
                    (set! choice (get-field (_ "subsubtitle") subsubtitle))
                    (if choice (set! subsubtitle choice)))
                ((instrument)
                    (if instrument (set! editing #t))                
                    (set! choice (get-field (_ "instrument") instrument))
                    (if choice (set! instrument choice)))
                ((poet)
                    (if poet (set! editing #t))                
                    (set! choice (get-field (_ "poet") poet))
                    (if choice (set! poet choice)))
                ((composer)
                    (if composer (set! editing #t))
                    (set! choice (get-field (_ "composer") composer))
                    (if choice (set! composer choice)))
                ((meter)
                            (if meter (set! editing #t))                
                            (set! choice (get-field (_ "meter") meter))
                    (if choice (set! meter choice)))
                ((arranger)
                    (if arranger (set! editing #t))                
                    (set! choice (get-field (_ "arranger") arranger))
                    (if choice (set! arranger choice)))
                ((tagline)
                    (if tagline (set! editing #t))                
                    (set! choice (get-field (_ "tagline") tagline))
                    (if choice (set! tagline choice)))
                ((copyright)
                    (if copyright (set! editing #t))                
                    (set! choice (get-field (_ "copyright") copyright))
                    (if choice (set! copyright choice)))
                ((piece)
                    (if piece (set! editing #t))                
                    (set! choice (get-field (_ "piece") piece))
                    (if choice (set! piece choice)))
                ((opus)
                    (if opus (set! editing #t))                
                    (set! choice (get-field (_ "opus") opus))
                    (if choice (set! opus choice))))
                    
             
             (if (not (eq? choice 'abort))
                (let ((thealist (string-append "(list " 
                    (form-pair "dedication" dedication) 
                    (form-pair "title" title)
                    (form-pair "subtitle" subtitle)
                    (form-pair "subsubtitle" subsubtitle)
                    (form-pair "instrument" instrument)
                    (form-pair "poet" poet)
                    (form-pair "composer" composer)
                    (form-pair "meter" meter)
                    (form-pair "arranger" arranger)
                    (form-pair "tagline" tagline)
                    (form-pair "copyright" copyright)
                    (form-pair "piece" piece)
                    (form-pair "opus" opus) " '())")))
                    (if score
                        (d-DirectivePut-scoreheader-data tag thealist)
                        (d-DirectivePut-header-data tag thealist))
                    (write-titles))))))
                    
;;;;;;;;;;;
(define (DenemoSetVerticalSpacingDist tag type title default)
    (let ((data (d-DirectiveGet-paper-data tag)))
        (if (not data)
            (set! data default))
        (set! data (d-GetUserInput title (_ "Give Spacing:")  data))
        (if (and data (string->number data))
            (begin
                (d-SetSaved #f)
                (d-DirectivePut-paper-data tag data)
                (d-DirectivePut-paper-postfix tag (string-append type ".basic-distance =  " data "\n"))))))
                
;;;;edit staff, called from tools icon at start of staff
(define (EditStaff)
        (let* ((num (number->string (d-GetStaff)))(choice (RadioBoxMenu 
            (if  (> (d-StaffMasterVolume) 0)
                (cons (string-append (_ "Mute Staff") " " num) 'mute)
                (cons  (string-append (_ "Unmute Staff") " " num) 'mute))
            (if (d-Directive-staff? "NonPrintingStaff")
                (cons  (string-append (_ "(Print) Show Staff") " " num) 'show)
                (cons  (string-append (_ "(Print) Hide Staff") " " num) 'show))
                
            (if (d-StaffHidden)
                (cons  (string-append (_ "(Display) Show Staff") " " num) 'display)
                (cons  (string-append (_ "(Display) Hide Staff") " " num) 'display))
                
            (cons (_ "Built-in Staff Properties") 'editor))))
        (case choice
            ((mute) (d-MuteStaff))
            ((show) (d-NonPrintingStaff))
            ((display) (d-ToggleCurrentStaffDisplay))
            ((editor) (d-StaffProperties)))))
;;;;edit staff, called from tools icon at start of staff
(define (EditMovement)
        (let* ((choice (RadioBoxMenu 
            (cons (_ "Help") 'help)
            (cons (_ "Mute Staffs") 'mute)
            (cons (_ "(Display) Show All Staffs") 'show)
            (cons (_ "(Display) Hide All Other Staffs") 'hide)
            (cons (_ "Movement Editor") 'editor))))
        (case choice
            ((help) (d-InfoDialog (_ "This sets the visibility/mute and other properties for the whole movement")))
            ((mute) (d-MuteStaffs))
            ((show) (StaffsVisibility #t))
            ((hide)  (StaffsVisibility #f))
            ((editor) (d-EditMovementProperties)))))
;;;
(define (StaffsVisibility bool)
(d-PushPosition)
(while (d-MoveToStaffUp))
(let loop () (d-StaffHidden (not bool))
    (if (d-MoveToStaffDown) 
        (loop)))
(d-PopPosition))      

;;;;;
(define (DenemoInsertChordTransposed notes root-note) ;; notes is a string e.g. "c e g" root-note is a symbol e.g 'cis,,
    (let ((cursorNote #f)(above #f)(interval #f)(old_volume (d-MasterVolume)))
      (d-MasterVolume 0)
      (d-PutNote)
      (d-MoveCursorLeft)
      (set! cursorNote (string->symbol (d-GetNote)))
      (d-DeleteObject)
      (d-InsertChord notes)
      (d-MoveCursorLeft)
      (set! above (< (ANS::Ly2Ans cursorNote)  (ANS::Ly2Ans root-note)))
      (set! interval (ANS::GetInterval  (ANS::Ly2Ans root-note)  (ANS::Ly2Ans cursorNote)))
      (ANS::ChangeChordNotes (map (lambda (x) ((if above ANS::IntervalCalcDown ANS::IntervalCalcUp) x interval)) (ANS::GetChordNotes)))
      (d-MasterVolume old_volume)
      (d-PlayAtCursor)))       

(define (DenemoGetNoteAndAccidental)
    (define note (d-GetNote 1))
    (define acc "")
    (define name #f)
    (if note
        (begin
            (set! name (string-upcase (substring note 0 1)))
            (if (> (string-length note) 1)
                (let ((char (string-ref note 1)))
                    (if (eq? char #\e)
                        (set! acc "♭")
                        (if (eq? char #\i)
                            (set! acc "♯")))))
        (string-append name acc))
        #f))

(define* (DenemoGetUserNumberAsString #:optional (title "") (prompt "") (initial ""))
    (let ((val (d-GetUserInput title prompt initial)))
        (if (and val (string->number val))
            val
            #f)))
            
(define-once Transpose::Interval "c g");;; a more sensible default used by all typeset-transposed functions


        
        
