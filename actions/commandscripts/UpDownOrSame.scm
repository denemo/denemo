;;;;;;;;;;;;;;;
;;UpDownOrSame

(define UpDownOrSame::score 0)
(define UpDownOrSame::interval 2)
(define UpDownOrSame::start (current-time))
(define UpDownOrSame::end (current-time))
(define UpDownOrSame::num-goes 30)
(define UpDownOrSame::directionwas #t)
(define UpDownOrSame::userinput #t)
(define UpDownOrSame::acceptable_list (list "Up" "Down" "Right"))

(define (UpDownOrSame::showscore)
  (d-DirectivePut-score-display "UpDownOrSame::GameScore" (string-append "<b>Score: " (object->string UpDownOrSame::score) "</b> in " (object->string (- UpDownOrSame::end UpDownOrSame::start)) " Secs.")))

(define (UpDownOrSame::help) 
  (d-InfoDialog "Click on the Up arrow if the note goes up, 
  Down arrow if the note goes down, and Right arrow if the note stays the same")
)

(define (UpDownOrSame::GameOver)
   (d-InfoDialog (string-append "Game Over\n" "Your Score = " (number->string UpDownOrSame::score)))
)

(define (UpDownOrSame::ScoreBoard)
   (d-InfoDialog (EducationGames::Scoreboard_Pretty_Print
         (EducationGames::ScoreboardFile "UpDownOrSame")))
)

(CreateButton "UpDownOrSame::GameScore" "<span font_desc=\"12\">Score</span>")
(d-SetDirectiveTagActionScript "UpDownOrSame::GameScore" "(UpDownOrSame::ScoreBoard)")
(CreateButton "UpDownOrSame::GameHelp" "<b>Help</b>")
(d-SetDirectiveTagActionScript "UpDownOrSame::GameHelp" "(UpDownOrSame::help)")


;;;;;;;; the main function to run the test - just goes to end and places a note at a random height above the cursor, returning the cursor to where it was.
(define (UpDownOrSame::offerNote) 
(if (>= UpDownOrSame::num-goes 0)
	(let (    
	      (steps 0)
	      (direction 0)
	      )
	  (d-MoveToEnd)
	  (set! steps (+ 1 (random UpDownOrSame::interval)))
	  (set! direction (random 3))
	  (cond ((= direction 0)
	            (EducationGames::shiftdown steps)
                    (d-Insert2) 
                    (set! UpDownOrSame::directionwas "Down"))
          	((= direction 1)
	            (EducationGames::shiftup steps)
                    (d-Insert2)
	            (set! UpDownOrSame::directionwas "Up"))
	        ((= direction 2)
                    (d-Insert2)
	            (set! UpDownOrSame::directionwas "Same")))
	   (UpDownOrSame::showscore)
	   (set! UpDownOrSame::num-goes (-  UpDownOrSame::num-goes 1))
	  )
	))

;;;;;;;;; callback when user chooses a note
(define (UpDownOrSame::directionchosen usernote)
		      (begin
			(set! UpDownOrSame::end (current-time))
			(d-MoveToEnd)
			(if  (string=? UpDownOrSame::directionwas usernote)
			     (begin
			       (set! UpDownOrSame::score (+ UpDownOrSame::score 1))
			       (EducationGames::PlaceAnswerStatus "CheckMark"))
			     (begin
			       (set! UpDownOrSame::score (- UpDownOrSame::score 1))
			       (EducationGames::PlaceAnswerStatus "CrossSign"))) 
			       (UpDownOrSame::offerNote)
			       ))
	
;;;;;;;; the main function to run the test
(define UpDownOrSame::runtest 
	(lambda (n)
	  (if (>= n 0) (begin
	    		(set! UpDownOrSame::userinput (EducationGames::GetAcceptableKeyInput UpDownOrSame::acceptable_list))
			(if (string=? UpDownOrSame::userinput "Right")
			  (set! UpDownOrSame::userinput "Same"))
			(UpDownOrSame::directionchosen UpDownOrSame::userinput)	
			(UpDownOrSame::runtest UpDownOrSame::num-goes)))))

(EducationGames::Chime)
			
(define (UpDownOrSame::createbuttons direction)
  (CreateButton (string-append "UpDownOrSame::" direction)  (string-append " <span font_desc=\"32\" foreground=\"blue\">" direction  "</span>"))
  (d-SetDirectiveTagActionScript  (string-append "UpDownOrSame::" direction) (string-append "(UpDownOrSame::directionchosen \"" direction "\")")))

(UpDownOrSame::createbuttons "Up")
(UpDownOrSame::createbuttons "Down")
(UpDownOrSame::createbuttons "Same")

(EducationGames::shiftup 6)
(d-Insert2)
(UpDownOrSame::offerNote)
(set! UpDownOrSame::start (current-time))
(UpDownOrSame::runtest UpDownOrSame::num-goes)
(if (not                                                                                                                                           (EducationGames::Write_Scoreboard_File
  (EducationGames::ScoreboardFile "UpDownOrSame") UpDownOrSame::score))
  (UpDownOrSame::GameOver)
  (UpDownOrSame::ScoreBoard)
  )

(EducationGames::Chime)

;;;;;;;;;;;;;;;;;;;;;;;;;;

