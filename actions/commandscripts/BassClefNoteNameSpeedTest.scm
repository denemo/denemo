;;;;;;;;;;;;;;;
;;BassClefNoteNameSpeedTest NEW VERSION
;; tests note name recognition.


(define BassClefNoteNameSpeedTest::score 0)
(define BassClefNoteNameSpeedTest::note_position -4) ;0 = middle c
(define BassClefNoteNameSpeedTest::note_highest 1)
(define BassClefNoteNameSpeedTest::note_lowest -12)
(define BassClefNoteNameSpeedTest::clef "Bass")
(define BassClefNoteNameSpeedTest::start 0)
(define BassClefNoteNameSpeedTest::end 0)
(define BassClefNoteNameSpeedTest::TimeLimit 120)
(define BassClefNoteNameSpeedTest::notewas #f)
(define BassClefNoteNameSpeedTest::LastNoteCorrect? #t)
(define BassClefNoteNameSpeedTest::ExtraChances 2)
(define BassClefNoteNameSpeedTest::acceptable_list (list "a" "b" "c" "d" "e" "f" "g"))

(define (BassClefNoteNameSpeedTest::ElapsedTime)
  (- BassClefNoteNameSpeedTest::end BassClefNoteNameSpeedTest::start))

(define (BassClefNoteNameSpeedTest::showscore)
  (d-DirectivePut-score-display "BassClefNoteNameSpeedTest::GameScore" (string-append "<b>Score: " 
    (object->string BassClefNoteNameSpeedTest::score) "</b> in " 
    (object->string (BassClefNoteNameSpeedTest::ElapsedTime)) " Secs.")))

(define (BassClefNoteNameSpeedTest::GameOverDialog)
   (d-InfoDialog (string-append "Game Over\n" "Your Score = " (number->string BassClefNoteNameSpeedTest::score))))

(define (BassClefNoteNameSpeedTest::ScoreBoard)
    (d-InfoDialog (EducationGames::Scoreboard_Pretty_Print 
      (EducationGames::ScoreboardFile "BassClefNoteNameSpeedTest"))))

;#t give differnt note
(define (BassClefNoteNameSpeedTest::AnotherChance?)
  (and
    (not BassClefNoteNameSpeedTest::LastNoteCorrect?)
    (>=  BassClefNoteNameSpeedTest::ExtraChances 0)))

;;;;;;;;; callback when user chooses a note
(define (BassClefNoteNameSpeedTest::notechosen usernote)
		      (begin
			(set! BassClefNoteNameSpeedTest::end (current-time))
			(d-MoveToEnd)
			(if  (string=? BassClefNoteNameSpeedTest::notewas usernote)
			     (begin
			       (set! BassClefNoteNameSpeedTest::score (+ BassClefNoteNameSpeedTest::score 1))
			       (set! BassClefNoteNameSpeedTest::LastNoteCorrect? #t)
			       (EducationGames::PlaceAnswerStatus "CheckMark"))
			     (begin
			       (set! BassClefNoteNameSpeedTest::score (- BassClefNoteNameSpeedTest::score 1))
			       (set! BassClefNoteNameSpeedTest::LastNoteCorrect? #f)
			       (set! BassClefNoteNameSpeedTest::ExtraChances (- BassClefNoteNameSpeedTest::ExtraChances 1))
			       (EducationGames::PlaceAnswerStatus "CrossSign"))
			       ) 
			(BassClefNoteNameSpeedTest::offerNote)))


(define (BassClefNoteNameSpeedTest::print)
 (d-DeletePreviousObject)
 (SetHeaderField "title" "Note Naming Game")
 (SetHeaderField "subtitle" (string-append "Score was: "  (object->string BassClefNoteNameSpeedTest::score) 
  " in "  (object->string (- BassClefNoteNameSpeedTest::end BassClefNoteNameSpeedTest::start)) " Seconds"))
 (SetHeaderField "piece" "user: anonymous")
 (DenemoPrintAllHeaders)
 (d-PrintPreview)
 (d-DirectiveDelete-header "Movement-title")
 (d-DirectiveDelete-header "Movement-subtitle")
 (d-DirectiveDelete-header "Movement-piece")
 (BassClefNoteNameSpeedTest::offerNote)
 (d-SetSaved))

(define (BassClefNoteNameSpeedTest::help) 
  (d-InfoDialog "Identify the note that appears on the staff. Then hit the notename on the keyboard. Be quick because you have a time limit!!!"))

;;;
(define (BassClefNoteNameSpeedTest::GetKeyBoardInput) 
	(if (>= (BassClefNoteNameSpeedTest::ElapsedTime) BassClefNoteNameSpeedTest::TimeLimit)
	  (begin
	    (BassClefNoteNameSpeedTest::GameOver))
	  (begin
            (BassClefNoteNameSpeedTest::notechosen (EducationGames::GetAcceptableKeyInput BassClefNoteNameSpeedTest::acceptable_list))
	    (BassClefNoteNameSpeedTest::GetKeyBoardInput)))
	  )

(define (BassClefNoteNameSpeedTest::go)
  (if (not (zero? BassClefNoteNameSpeedTest::score))
      (let ((response #f))
	(set! response (d-GetUserInput "Reset Score" "Do you want to reset your score" "y"))
	(if (equal? response "y")
	      (set! BassClefNoteNameSpeedTest::score 0))))
  
  (d-DeletePreviousObject)
  (d-InitialClef BassClefNoteNameSpeedTest::clef)
  (EducationGames::Chime)
  (set! BassClefNoteNameSpeedTest::start (current-time))
  (set! BassClefNoteNameSpeedTest::end (current-time))
  (BassClefNoteNameSpeedTest::offerNote)
  (BassClefNoteNameSpeedTest::GetKeyBoardInput))

(define (BassClefNoteNameSpeedTest::offerNote) 
	(let (    
	      (outofrange? 0)
	      (seed 0)
	      )
	      (set! outofrange?
	        (lambda ()
		  (or (> BassClefNoteNameSpeedTest::note_position BassClefNoteNameSpeedTest::note_highest) 
		      (> BassClefNoteNameSpeedTest::note_lowest BassClefNoteNameSpeedTest::note_position)) 
			))
	      (set! seed 
	        (lambda ()
	          (if (= (random 2) 0)
	            (set! BassClefNoteNameSpeedTest::note_position (+ BassClefNoteNameSpeedTest::note_position 1))
		    (set! BassClefNoteNameSpeedTest::note_position (- BassClefNoteNameSpeedTest::note_position 1)))
		    (if (outofrange?)
		      (seed))
		    ))
	  (d-MoveToEnd)
          (if (BassClefNoteNameSpeedTest::AnotherChance?)
  	    (begin
              (d-Insert1))
            (begin
              (seed)
	      (d-CursorToNote (EducationalGames::middle_c_offset->lily BassClefNoteNameSpeedTest::note_position))
	      (d-Insert1)
	      (set! BassClefNoteNameSpeedTest::ExtraChances 2)))
	  (set! BassClefNoteNameSpeedTest::notewas (d-GetNoteName))
	   
	  (BassClefNoteNameSpeedTest::showscore)
	  ))


(CreateButton "BassClefNoteNameSpeedTest::GameScore" "<span font_desc=\"22\">Score</span>")
(d-SetDirectiveTagActionScript "BassClefNoteNameSpeedTest::GameScore" "(BassClefNoteNameSpeedTest::ScoreBoard)")

(CreateButton "BassClefNoteNameSpeedTest::GameHelp" "<b>Help</b>")
(d-SetDirectiveTagActionScript "BassClefNoteNameSpeedTest::GameHelp" "(BassClefNoteNameSpeedTest::help)")

(CreateButton "BassClefNoteNameSpeedTest::GameGo" "<span font_desc=\"22\">Start</span>")
(d-SetDirectiveTagActionScript "BassClefNoteNameSpeedTest::GameGo" "(BassClefNoteNameSpeedTest::go)")

(CreateButton "BassClefNoteNameSpeedTest::GamePrint" "<span font_desc=\"22\">Print</span>")
(d-SetDirectiveTagActionScript "BassClefNoteNameSpeedTest::GamePrint" "(BassClefNoteNameSpeedTest::print)")


(define (BassClefNoteNameSpeedTest::GameOver)
   (EducationGames::Chime)
    (if (not 
      (EducationGames::Write_Scoreboard_File 
      (EducationGames::ScoreboardFile "BassClefNoteNameSpeedTest") BassClefNoteNameSpeedTest::score))
      (BassClefNoteNameSpeedTest::GameOverDialog)
      (BassClefNoteNameSpeedTest::ScoreBoard)
    ))
