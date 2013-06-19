;;;;;;;;;;;;;;;
;;NoteNameSpeedTest NEW VERSION
;; tests note name recognition.


(define NoteNameSpeedTest::score 0)
(define NoteNameSpeedTest::note_position 6) ;0 = middle c
(define NoteNameSpeedTest::note_highest 11)
(define NoteNameSpeedTest::note_lowest 0)
(define NoteNameSpeedTest::start 0)
(define NoteNameSpeedTest::end 0)
(define NoteNameSpeedTest::TimeLimit 120)
(define NoteNameSpeedTest::notewas #f)
(define NoteNameSpeedTest::LastNoteCorrect? #t)
(define NoteNameSpeedTest::ExtraChances 2)
(define NoteNameSpeedTest::acceptable_list (list "a" "b" "c" "d" "e" "f" "g"))

(define (NoteNameSpeedTest::ElapsedTime)
  (- NoteNameSpeedTest::end NoteNameSpeedTest::start))

(define (NoteNameSpeedTest::showscore)
  (d-DirectivePut-score-display "NoteNameSpeedTest::GameScore" (string-append "<b>Score: " 
    (object->string NoteNameSpeedTest::score) "</b> in " 
    (object->string (NoteNameSpeedTest::ElapsedTime)) " Secs.")))

(define (NoteNameSpeedTest::GameOverDialog)
   (d-InfoDialog (string-append "Game Over\n" "Your Score = " (number->string NoteNameSpeedTest::score))))

(define (NoteNameSpeedTest::ScoreBoard)
    (d-InfoDialog (EducationGames::Scoreboard_Pretty_Print 
      (EducationGames::ScoreboardFile "NoteNameSpeedTest"))))
      
;#t give differnt note
(define (NoteNameSpeedTest::AnotherChance?)
  (and
    (not NoteNameSpeedTest::LastNoteCorrect?)
    (>=  NoteNameSpeedTest::ExtraChances 0)))
    
;;;;;;;;; callback when user chooses a note
(define (NoteNameSpeedTest::notechosen usernote)
		      (begin
			(set! NoteNameSpeedTest::end (current-time))
			(d-MoveToEnd)
	  	        (if  (string=? NoteNameSpeedTest::notewas usernote)
			     (begin
			       (set! NoteNameSpeedTest::score (+ NoteNameSpeedTest::score 1))
			       (set! NoteNameSpeedTest::LastNoteCorrect? #t)
			       (EducationGames::PlaceAnswerStatus "CheckMark"))
			     (begin
			       (set! NoteNameSpeedTest::score (- NoteNameSpeedTest::score 1))
			       (set! NoteNameSpeedTest::LastNoteCorrect? #f)
			       (set! NoteNameSpeedTest::ExtraChances (- NoteNameSpeedTest::ExtraChances 1))
			       (EducationGames::PlaceAnswerStatus "CrossSign"))) 
			(NoteNameSpeedTest::offerNote)))


(define (NoteNameSpeedTest::print)
 (d-DeletePreviousObject)
 (SetHeaderField "title" "Note Naming Game")
 (SetHeaderField "subtitle" (string-append "Score was: "  (object->string NoteNameSpeedTest::score) 
  " in "  (object->string (- NoteNameSpeedTest::end NoteNameSpeedTest::start)) " Seconds"))
 (SetHeaderField "piece" "user: anonymous")
 (DenemoPrintAllHeaders)
 (d-PrintPreview)
 (d-DirectiveDelete-header "Movement-title")
 (d-DirectiveDelete-header "Movement-subtitle")
 (d-DirectiveDelete-header "Movement-piece")
 (NoteNameSpeedTest::offerNote)
 (d-SetSaved))

(define (NoteNameSpeedTest::help) 
  (d-InfoDialog "Identify the note that appears on the staff. Then hit the notename on the keyboard. Be quick because you have a time limit!!!"))

;;;
(define (NoteNameSpeedTest::GetKeyBoardInput) 
	(if (>= (NoteNameSpeedTest::ElapsedTime) NoteNameSpeedTest::TimeLimit)
	  (begin
	    (NoteNameSpeedTest::GameOver))
	  (begin
            (NoteNameSpeedTest::notechosen (EducationGames::GetAcceptableKeyInput NoteNameSpeedTest::acceptable_list))
	    (NoteNameSpeedTest::GetKeyBoardInput)))
	  )

(define (NoteNameSpeedTest::go)
  (if (not (zero? NoteNameSpeedTest::score))
      (let ((response #f))
	(set! response (d-GetUserInput "Reset Score" "Do you want to reset your score" "y"))
	(if (equal? response "y")
	      (set! NoteNameSpeedTest::score 0))))
  
  (d-DeletePreviousObject)
  (EducationGames::Chime)
  (set! NoteNameSpeedTest::start (current-time))
  (set! NoteNameSpeedTest::end (current-time))
  (NoteNameSpeedTest::offerNote)
  (NoteNameSpeedTest::GetKeyBoardInput))

(define (NoteNameSpeedTest::offerNote) 
	(let (    
	      (outofrange? 0)
	      (seed 0)
	      )
	      (set! outofrange?
	        (lambda ()
		  (or (> NoteNameSpeedTest::note_position NoteNameSpeedTest::note_highest) 
		      (> NoteNameSpeedTest::note_lowest NoteNameSpeedTest::note_position)) 
			))
	      (set! seed 
	        (lambda ()
	          (if (= (random 2) 0)
	            (set! NoteNameSpeedTest::note_position (+ NoteNameSpeedTest::note_position 1))
		    (set! NoteNameSpeedTest::note_position (- NoteNameSpeedTest::note_position 1)))
		    (if (outofrange?)
		      (seed))
		    ))

	  (d-MoveToEnd) 
	  (if (NoteNameSpeedTest::AnotherChance?)
	    (begin
	      (d-Insert1))
	    (begin 
	      (seed)
	      (d-CursorToNote (EducationalGames::middle_c_offset->lily NoteNameSpeedTest::note_position))
	      (d-Insert1)
	      (set! NoteNameSpeedTest::ExtraChances 2)))
	  (set! NoteNameSpeedTest::notewas (d-GetNoteName))
	  (NoteNameSpeedTest::showscore)
	  ))


(CreateButton "NoteNameSpeedTest::GameScore" "<span font_desc=\"22\">Score</span>")
(d-SetDirectiveTagActionScript "NoteNameSpeedTest::GameScore" "(NoteNameSpeedTest::ScoreBoard)")

(CreateButton "NoteNameSpeedTest::GameHelp" "<b>Help</b>")
(d-SetDirectiveTagActionScript "NoteNameSpeedTest::GameHelp" "(NoteNameSpeedTest::help)")

(CreateButton "NoteNameSpeedTest::GameGo" "<span font_desc=\"22\">Start</span>")
(d-SetDirectiveTagActionScript "NoteNameSpeedTest::GameGo" "(NoteNameSpeedTest::go)")

(CreateButton "NoteNameSpeedTest::GamePrint" "<span font_desc=\"22\">Print</span>")
(d-SetDirectiveTagActionScript "NoteNameSpeedTest::GamePrint" "(NoteNameSpeedTest::print)")


(define (NoteNameSpeedTest::GameOver)
   (EducationGames::Chime)
    (if (not 
      (EducationGames::Write_Scoreboard_File 
      (EducationGames::ScoreboardFile "NoteNameSpeedTest") NoteNameSpeedTest::score))
      (NoteNameSpeedTest::GameOverDialog)
      (NoteNameSpeedTest::ScoreBoard)
    ))
