;;;;;;;;;;;;;;;
;;ReadingNoteNames NEW VERSION
;; tests note name recognition.


(define ReadingNoteNames::score 0)
(define ReadingNoteNames::interval 8)
(define ReadingNoteNames::start (current-time))
(define ReadingNoteNames::end (current-time))
(define ReadingNoteNames::note_position 0)
(define ReadingNoteNames::notewas #f)
(define ReadingNoteNames::LastNoteCorrect? #t)
(define ReadingNoteNames::ExtraChances 2)

(define (ReadingNoteNames::showscore)
  (d-DirectivePut-score-display "ReadingNoteNames::GameScore" (string-append "<b>Score: " (object->string ReadingNoteNames::score) "</b> in " (object->string (- ReadingNoteNames::end ReadingNoteNames::start)) " Secs.")))

;#t give differnt note
(define (ReadingNoteNames::AnotherChance?)
  (and
    (not ReadingNoteNames::LastNoteCorrect?)
    (>=  ReadingNoteNames::ExtraChances 0)))


;;;;;;;;; callback when user chooses a note
(define (ReadingNoteNames::notechosen usernote)
		      (begin
			(set! ReadingNoteNames::end (current-time))
			;(let gotoEnd () (if  (d-NextObject) (gotoEnd)))
			(EducationGames::gotoEnd)
			(if  (string=? ReadingNoteNames::notewas usernote)
			     (begin
			       (set! ReadingNoteNames::score (+ ReadingNoteNames::score 1))
			       (set! ReadingNoteNames::LastNoteCorrect? #t)
			       (EducationGames::PlaceAnswerStatus "CheckMark"))
			     (begin
			       (set! ReadingNoteNames::score (- ReadingNoteNames::score 1))
			       (set! ReadingNoteNames::LastNoteCorrect? #f)
			       (set! ReadingNoteNames::ExtraChances (- ReadingNoteNames::ExtraChances 1))
			       (EducationGames::PlaceAnswerStatus "CrossSign"))
			       ) 
			(ReadingNoteNames::offerNote)))


(define (ReadingNoteNames::print)
 (d-DeletePreviousObject)
 (SetHeaderField "title" "Note Naming Game")
 (SetHeaderField "subtitle" (string-append "Score was: "  (object->string ReadingNoteNames::score) 
  " in "  (object->string (- ReadingNoteNames::end ReadingNoteNames::start)) " Seconds"))
 (SetHeaderField "piece" "user: anonymous")
 (DenemoPrintAllHeaders)
 (d-PrintPreview)
 (d-DirectiveDelete-header "Movement-title")
 (d-DirectiveDelete-header "Movement-subtitle")
 (d-DirectiveDelete-header "Movement-piece")
 (ReadingNoteNames::offerNote)
 (d-SetSaved))
(define (ReadingNoteNames::quit) 
  (d-Close))
(define (ReadingNoteNames::help) 
  (d-InfoDialog "Click on the bottom of the range of notes you want to learn.
Use the Interval button to choose how many notes above that you want to try.
Print out your score when you have finished.")
)

(define (ReadingNoteNames::plus)
  (set! ReadingNoteNames::interval (+ ReadingNoteNames::interval 1))
  (if (> ReadingNoteNames::interval 32)
      (set! ReadingNoteNames::interval 32))
  (d-DirectivePut-score-display "ReadingNoteNames::GameInterval"
				(string-append "<span font_desc=\"12\">Interval: " (object->string ReadingNoteNames::interval) "</span>")))

(define (ReadingNoteNames::minus)
  (set! ReadingNoteNames::interval (- ReadingNoteNames::interval 1))
  (if (< ReadingNoteNames::interval 2)
      (set! ReadingNoteNames::interval 2))
  (d-DirectivePut-score-display "ReadingNoteNames::GameInterval"
				(string-append "<span font_desc=\"12\">Interval: " (object->string ReadingNoteNames::interval) "</span>")))

(define (ReadingNoteNames::setInterval)
  (set! ReadingNoteNames::interval (string->number (d-GetUserInput "Interval" "Give number of steps you want to name note over" (object->string ReadingNoteNames::interval))))
  (if (boolean? ReadingNoteNames::interval)
       (set! ReadingNoteNames::interval 8))
  (if (< ReadingNoteNames::interval 2)
       (set! ReadingNoteNames::interval 2))
  (if (> ReadingNoteNames::interval 32)
      (set! ReadingNoteNames::interval 32))
  (d-DirectivePut-score-display "ReadingNoteNames::GameInterval"
				(string-append "<span font_desc=\"12\">Interval: " (object->string ReadingNoteNames::interval) "</span>")))


;;;
(define (ReadingNoteNames::go)
  (if (not (zero? ReadingNoteNames::score))
      (let ((response #f))
	(set! response (d-GetUserInput "Reset Score" "Do you want to reset your score" "y"))
	(if (equal? response "y")
	    (begin
	      (set! ReadingNoteNames::start (current-time))
	      (set! ReadingNoteNames::end (current-time))

	      (set! ReadingNoteNames::score 0)))))
  (d-DeletePreviousObject)
  (ReadingNoteNames::offerNote))



;;;;;;;; the main function to run the test - just goes to end and places a note at a random position relative to ReadingNoteNames::note_position.
(define (ReadingNoteNames::offerNote) 
    (if #t
	(let (    
	      (usernote #t)
	      (steps 0)
	      
	      )
	  (EducationGames::gotoEnd)
	  (if (ReadingNoteNames::AnotherChance?)
	    (begin
	      ;(d-CursorToNote (EducationalGames::middle_c_offset->lily ReadingNoteNames::note_position))
	      (d-Insert2))
	    (begin
	      (set! steps (+ 1 (random ReadingNoteNames::interval)))      
	      ;(EducationGames::shiftup steps)
	      (set! ReadingNoteNames::note_position (+ ReadingNoteNames::note_position steps))
	      (d-CursorToNote (EducationalGames::middle_c_offset->lily ReadingNoteNames::note_position))
	      (d-Insert2)
	      (set! ReadingNoteNames::ExtraChances 2)))
	  (set! ReadingNoteNames::notewas (d-GetNoteName))	
	  (ReadingNoteNames::showscore)
	  (if ReadingNoteNames::LastNoteCorrect?
	    (set! ReadingNoteNames::note_position (- ReadingNoteNames::note_position steps)))
	  )
	))
(EducationGames::Chime)

(d-New)
(d-InitialClef)
(let ((clef (d-GetPrevailingClef)))
  (cond
    ((equal? clef "Alto")
      (set! ReadingNoteNames::note_position -6))
    ((equal? clef "Tenor")
      (set! ReadingNoteNames::note_position -8))
   ((equal? clef "Bass")
      (set! ReadingNoteNames::note_position -11))
   ((equal? clef "French")
      (set! ReadingNoteNames::note_position 3))
      ))
(d-DestroyButtons)
(CreateButton "ReadingNoteNames::GameScore" "<span font_desc=\"32\">Click to start</span>")
(CreateButton "ReadingNoteNames::GameHelp" "<b>Help</b>")
(d-SetDirectiveTagActionScript "ReadingNoteNames::GameHelp" "(ReadingNoteNames::help)")



(CreateButton "ReadingNoteNames::GamePlus" "<b>+</b>")
(d-SetDirectiveTagActionScript "ReadingNoteNames::GamePlus" "(ReadingNoteNames::plus)")
(CreateButton "ReadingNoteNames::GameInterval" (string-append "<span font_desc=\"12\">Interval: " (object->string ReadingNoteNames::interval) "</span>"))
(d-SetDirectiveTagActionScript "ReadingNoteNames::GameInterval" "(ReadingNoteNames::setInterval)")
(CreateButton "ReadingNoteNames::GameMinus" "<b>-</b>")
(d-SetDirectiveTagActionScript "ReadingNoteNames::GameMinus" "(ReadingNoteNames::minus)")




(CreateButton "ReadingNoteNames::GameGo" "<span font_desc=\"32\">Re-start</span>")
(d-SetDirectiveTagActionScript "ReadingNoteNames::GameGo" "(ReadingNoteNames::go)")
(d-SetDirectiveTagActionScript "ReadingNoteNames::GameScore" "(ReadingNoteNames::go)")


(CreateButton "ReadingNoteNames::GamePrint" "<span font_desc=\"32\">Print</span>")
(d-SetDirectiveTagActionScript "ReadingNoteNames::GamePrint" "(ReadingNoteNames::print)")


(CreateButton "ReadingNoteNames::Close" "<span font_desc=\"32\">Quit</span>")
(d-SetDirectiveTagActionScript "ReadingNoteNames::Close" "(ReadingNoteNames::quit)")




(define (ReadingNoteNames::createbuttons note)
  (CreateButton (string-append "ReadingNoteNames::" note)  (string-append " <span font_desc=\"32\" foreground=\"blue\">" note  "</span>"))
  (d-SetDirectiveTagActionScript  (string-append "ReadingNoteNames::" note) (string-append "(ReadingNoteNames::notechosen \"" note "\")")))


(CreateButton "ReadingNoteNames::spacer1" "<span font_desc=\"28\">    </span>")
(d-SetDirectiveTagActionScript "ReadingNoteNames::spacer1" "(d-PlayMidiKey #xF03001)")

(ReadingNoteNames::createbuttons "a")
(ReadingNoteNames::createbuttons "b")
(ReadingNoteNames::createbuttons "c")
(ReadingNoteNames::createbuttons "d")
(ReadingNoteNames::createbuttons "e")
(ReadingNoteNames::createbuttons "f")
(ReadingNoteNames::createbuttons "g")

(CreateButton "ReadingNoteNames::spacer2" "<span font_desc=\"28\">    </span>")
(d-SetDirectiveTagActionScript "ReadingNoteNames::spacer2" "(d-PlayMidiKey #xF03001)")

(EducationGames::Chime)
