;;;;;;;;;;;;;;;
;;ReadingNoteNamesSolfege NEW VERSION
;; tests solfege note name recognition.


(define ReadingNoteNamesSolfege::score 0)
(define ReadingNoteNamesSolfege::interval 8)
(define ReadingNoteNamesSolfege::start (current-time))
(define ReadingNoteNamesSolfege::end (current-time))
(define ReadingNoteNamesSolfege::note_position 0)
(define ReadingNoteNamesSolfege::notewas #f)
(define ReadingNoteNamesSolfege::LastNoteCorrect? #t)
(define ReadingNoteNamesSolfege::ExtraChances 2)

(define (ReadingNoteNamesSolfege::showscore)
  (d-DirectivePut-score-display "ReadingNoteNamesSolfege::GameScore" (string-append "<b>Score: " (object->string ReadingNoteNamesSolfege::score) "</b> in " (object->string (- ReadingNoteNamesSolfege::end ReadingNoteNamesSolfege::start)) " Secs.")))

;#t give differnt note
(define (ReadingNoteNamesSolfege::AnotherChance?)
  (and
    (not ReadingNoteNamesSolfege::LastNoteCorrect?)
    (>=  ReadingNoteNamesSolfege::ExtraChances 0)))

;;;;;;;;; callback when user chooses a note
(define (ReadingNoteNamesSolfege::notechosen usernote)
		      (begin
			(set! ReadingNoteNamesSolfege::end (current-time))
			;(let gotoEnd () (if  (d-NextObject) (gotoEnd)))
			(EducationGames::gotoEnd)
			(if  (string=? ReadingNoteNamesSolfege::notewas usernote)
			     (begin
			       (set! ReadingNoteNamesSolfege::score (+ ReadingNoteNamesSolfege::score 1))
			       (set! ReadingNoteNamesSolfege::LastNoteCorrect? #t)
			       (EducationGames::PlaceAnswerStatus "CheckMark"))
			     (begin
			       (set! ReadingNoteNamesSolfege::score (- ReadingNoteNamesSolfege::score 1))
			       (set! ReadingNoteNamesSolfege::LastNoteCorrect? #f)
			       (set! ReadingNoteNamesSolfege::ExtraChances (- ReadingNoteNamesSolfege::ExtraChances 1))
			       (EducationGames::PlaceAnswerStatus "CrossSign"))
			       ) 
			(ReadingNoteNamesSolfege::offerNote)))


(define (ReadingNoteNamesSolfege::print)
 (d-DeletePreviousObject)
 (SetHeaderField "title" "Note Naming Game")
 (SetHeaderField "subtitle" (string-append "Score was: "  (object->string ReadingNoteNamesSolfege::score) 
  " in "  (object->string (- ReadingNoteNamesSolfege::end ReadingNoteNamesSolfege::start)) " Seconds"))
 (SetHeaderField "piece" "user: anonymous")
 (DenemoPrintAllHeaders)
 (d-PrintPreview)
 (d-DirectiveDelete-header "Movement-title")
 (d-DirectiveDelete-header "Movement-subtitle")
 (d-DirectiveDelete-header "Movement-piece")
 (ReadingNoteNamesSolfege::offerNote)
 (d-SetSaved))
(define (ReadingNoteNamesSolfege::quit) 
  (d-Close))
(define (ReadingNoteNamesSolfege::help) 
  (d-InfoDialog "Click on the bottom of the range of notes you want to learn.
Use the Interval button to choose how many notes above that you want to try.
Print out your score when you have finished.")
)

(define (ReadingNoteNamesSolfege::plus)
  (set! ReadingNoteNamesSolfege::interval (+ ReadingNoteNamesSolfege::interval 1))
  (if (> ReadingNoteNamesSolfege::interval 32)
      (set! ReadingNoteNamesSolfege::interval 32))
  (d-DirectivePut-score-display "ReadingNoteNamesSolfege::GameInterval"
				(string-append "<span font_desc=\"12\">Interval: " (object->string ReadingNoteNamesSolfege::interval) "</span>")))

(define (ReadingNoteNamesSolfege::minus)
  (set! ReadingNoteNamesSolfege::interval (- ReadingNoteNamesSolfege::interval 1))
  (if (< ReadingNoteNamesSolfege::interval 2)
      (set! ReadingNoteNamesSolfege::interval 2))
  (d-DirectivePut-score-display "ReadingNoteNamesSolfege::GameInterval"
				(string-append "<span font_desc=\"12\">Interval: " (object->string ReadingNoteNamesSolfege::interval) "</span>")))

(define (ReadingNoteNamesSolfege::setInterval)
  (set! ReadingNoteNamesSolfege::interval (string->number (d-GetUserInput "Interval" "Give number of steps you want to name note over" (object->string ReadingNoteNamesSolfege::interval))))
  (if (boolean? ReadingNoteNamesSolfege::interval)
       (set! ReadingNoteNamesSolfege::interval 8))
  (if (< ReadingNoteNamesSolfege::interval 2)
       (set! ReadingNoteNamesSolfege::interval 2))
  (if (> ReadingNoteNamesSolfege::interval 32)
      (set! ReadingNoteNamesSolfege::interval 32))
  (d-DirectivePut-score-display "ReadingNoteNamesSolfege::GameInterval"
				(string-append "<span font_desc=\"12\">Interval: " (object->string ReadingNoteNamesSolfege::interval) "</span>")))


;;;
(define (ReadingNoteNamesSolfege::go)
  (if (not (zero? ReadingNoteNamesSolfege::score))
      (let ((response #f))
	(set! response (d-GetUserInput "Reset Score" "Do you want to reset your score" "y"))
	(if (equal? response "y")
	    (begin
	      (set! ReadingNoteNamesSolfege::start (current-time))
	      (set! ReadingNoteNamesSolfege::end (current-time))

	      (set! ReadingNoteNamesSolfege::score 0)))))
  (d-DeletePreviousObject)
  (ReadingNoteNamesSolfege::offerNote))



;;;;;;;; the main function to run the test - just goes to end and places a note at a random height above the cursor, returning the cursor to where it was.
(define (ReadingNoteNamesSolfege::offerNote) 
    (if #t
	(let (    
	      (usernote #t)
	      (steps 0)
	      
	      )
	;;  (d-GoToEnd)
	  ;(let gotoEnd () (if  (d-NextObject) (gotoEnd)))
	  (EducationGames::gotoEnd)
	  (if (ReadingNoteNamesSolfege::AnotherChance?)
	    (begin
	      ;(d-CursorToNote (EducationalGames::middle_c_offset->lily ReadingNoteNamesSolfege::note_position))
	      (d-Insert2))
	    (begin
	      (set! steps (+ 1 (random ReadingNoteNamesSolfege::interval)))      
	      ;(EducationGames::shiftup steps)
	      (set! ReadingNoteNamesSolfege::note_position (+ ReadingNoteNamesSolfege::note_position steps))
	      (d-CursorToNote (EducationalGames::middle_c_offset->lily ReadingNoteNamesSolfege::note_position))
	      (d-Insert2)
	      (set! ReadingNoteNamesSolfege::ExtraChances 2)))
	  (set! ReadingNoteNamesSolfege::notewas (d-GetNoteName))	
	  (ReadingNoteNamesSolfege::showscore)
	  ;(EducationGames::shiftdown steps)
	  (if ReadingNoteNamesSolfege::LastNoteCorrect?
	    (set! ReadingNoteNamesSolfege::note_position (- ReadingNoteNamesSolfege::note_position steps)))
	  )
	))
(EducationGames::Chime)

(d-New)

(CreateButton "ReadingNoteNamesSolfege::GameScore" "<span font_desc=\"24\">Click to start</span>")
(CreateButton "ReadingNoteNamesSolfege::GameHelp" "<b>Help</b>")
(d-SetDirectiveTagActionScript "ReadingNoteNamesSolfege::GameHelp" "(ReadingNoteNamesSolfege::help)")



(CreateButton "ReadingNoteNamesSolfege::GamePlus" "<b>+</b>")
(d-SetDirectiveTagActionScript "ReadingNoteNamesSolfege::GamePlus" "(ReadingNoteNamesSolfege::plus)")
(CreateButton "ReadingNoteNamesSolfege::GameInterval" (string-append "<span font_desc=\"12\">Interval: " (object->string ReadingNoteNamesSolfege::interval) "</span>"))
(d-SetDirectiveTagActionScript "ReadingNoteNamesSolfege::GameInterval" "(ReadingNoteNamesSolfege::setInterval)")
(CreateButton "ReadingNoteNamesSolfege::GameMinus" "<b>-</b>")
(d-SetDirectiveTagActionScript "ReadingNoteNamesSolfege::GameMinus" "(ReadingNoteNamesSolfege::minus)")




(CreateButton "ReadingNoteNamesSolfege::GameGo" "<span font_desc=\"24\">start</span>")
(d-SetDirectiveTagActionScript "ReadingNoteNamesSolfege::GameGo" "(ReadingNoteNamesSolfege::go)")
(d-SetDirectiveTagActionScript "ReadingNoteNamesSolfege::GameScore" "(ReadingNoteNamesSolfege::go)")


(CreateButton "ReadingNoteNamesSolfege::GamePrint" "<span font_desc=\"24\">Print</span>")
(d-SetDirectiveTagActionScript "ReadingNoteNamesSolfege::GamePrint" "(ReadingNoteNamesSolfege::print)")


(CreateButton "ReadingNoteNamesSolfege::Close" "<span font_desc=\"24\">Quit</span>")
(d-SetDirectiveTagActionScript "ReadingNoteNamesSolfege::Close" "(ReadingNoteNamesSolfege::quit)")




(define (ReadingNoteNamesSolfege::createbuttons solfege note)
(CreateButton (string-append "ReadingNoteNamesSolfege::" solfege)  (string-append " <span font_desc=\"24\" foreground=\"blue\">" solfege  "</span>"))
  (d-SetDirectiveTagActionScript  (string-append "ReadingNoteNamesSolfege::" solfege) (string-append "(ReadingNoteNamesSolfege::notechosen \"" note "\")")))


(CreateButton "ReadingNoteNamesSolfege::spacer1" "<span font_desc=\"28\">    </span>")
(d-SetDirectiveTagActionScript "ReadingNoteNamesSolfege::spacer1" "(d-PlayMidiKey #xF03001)")

(ReadingNoteNamesSolfege::createbuttons "do" "c")
(ReadingNoteNamesSolfege::createbuttons "re" "d")
(ReadingNoteNamesSolfege::createbuttons "me" "e")
(ReadingNoteNamesSolfege::createbuttons "fa" "f")
(ReadingNoteNamesSolfege::createbuttons "so" "g")
(ReadingNoteNamesSolfege::createbuttons "la" "a")
(ReadingNoteNamesSolfege::createbuttons "si" "b")

(CreateButton "ReadingNoteNamesSolfege::spacer2" "<span font_desc=\"28\">    </span>")
(d-SetDirectiveTagActionScript "ReadingNoteNamesSolfege::spacer2" "(d-PlayMidiKey #xF03001)")

(EducationGames::Chime)
