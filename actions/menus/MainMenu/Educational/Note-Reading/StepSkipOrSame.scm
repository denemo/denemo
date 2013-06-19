;;;;;;;;;;;;;;;
		;;StepSkipOrSame

;;StepSkipOrSame
;; tests interval recognition.
(define StepSkipOrSame::score 0)
(define StepSkipOrSame::interval 3)
(define StepSkipOrSame::start (current-time))
(define StepSkipOrSame::end (current-time))
(define StepSkipOrSame::num-goes 30)
(define StepSkipOrSame::intervalwas #t)
(define StepSkipOrSame::acceptable_list (list "a" "k" "t"))
(define StepSkipOrSame::stepstring #t)
(define StepSkipOrSame::userinput #t)
(define StepSkipOrSame::direction 0)

(define (StepSkipOrSame::showscore)
  (d-DirectivePut-score-display "StepSkipOrSame::GameScore" (string-append "<b>Score: " (object->string StepSkipOrSame::score) "</b> in " (object->string (- StepSkipOrSame::end StepSkipOrSame::start)) " Secs.")))

(define (StepSkipOrSame::help) 
  (d-InfoDialog "Is the music moving by a step, skip or staying the same. If the note is going up by skip hit the *k* key. If it is moving by step hit the *t* key. If it is staying the same hit the *a* key")
)

(define (StepSkipOrSame::GameOver)
   (d-InfoDialog (string-append "Game Over\n" "Your Score = " (number->string StepSkipOrSame::score)))
)

(define (StepSkipOrSame::ScoreBoard)
   (d-InfoDialog (EducationGames::Scoreboard_Pretty_Print
         (EducationGames::ScoreboardFile "StepSkipOrSame")))
)

(CreateButton "StepSkipOrSame::GameScore" "<span font_desc=\"12\">Score</span>")
(d-SetDirectiveTagActionScript "StepSkipOrSame::GameScore" "(StepSkipOrSame::ScoreBoard)")
(CreateButton "StepSkipOrSame::GameHelp" "<b>Help</b>")
(d-SetDirectiveTagActionScript "StepSkipOrSame::GameHelp" "(StepSkipOrSame::help)")


;;;;;;;; the main function to run the test - just goes to end and places a note at a random height above the cursor, returning the cursor to where it was.
(define (StepSkipOrSame::offerNote) 
(if (>= StepSkipOrSame::num-goes 0)
	(let ( (steps 0) )
	  (d-MoveToEnd)
	  (set! steps (random StepSkipOrSame::interval))
	  (if (= StepSkipOrSame::direction 0)
	      (begin
	            (EducationGames::shiftup steps)
	            (set! StepSkipOrSame::direction 1))
	      (begin 
	            (EducationGames::shiftdown steps)
	            (set! StepSkipOrSame::direction 0)))
	    
          (d-Insert2) 
	  	     
	  (if (= steps 0)
	      (begin
	        (set! StepSkipOrSame::stepstring "a")))
          (if (= steps 1)
	      (begin
		(set! StepSkipOrSame::stepstring "t")))
	  (if (>= steps 2)
	      (begin
		(set! StepSkipOrSame::stepstring "k")))

	  
          (StepSkipOrSame::showscore)
	  (set! StepSkipOrSame::num-goes (-  StepSkipOrSame::num-goes 1))
	  )
	))

;;;;;;;;; callback when user chooses a note
(define (StepSkipOrSame::intervalchosen interval)
		      (begin
			(set! StepSkipOrSame::end (current-time))
			(d-MoveToEnd)
			(if  (string=? StepSkipOrSame::stepstring interval)
			     (begin
			       (set! StepSkipOrSame::score (+ StepSkipOrSame::score 1))
			       (EducationGames::PlaceAnswerStatus "CheckMark"))
			     (begin
			       (set! StepSkipOrSame::score (- StepSkipOrSame::score 1))
			       (EducationGames::PlaceAnswerStatus "CrossSign"))) 
			       (StepSkipOrSame::offerNote)
			       ))
	
;;;;;;;; the main function to run the test
(define StepSkipOrSame::runtest 
	(lambda (n)
	  (if (>= n 0) (begin
	    		(set! StepSkipOrSame::userinput (EducationGames::GetAcceptableKeyInput StepSkipOrSame::acceptable_list))
			(StepSkipOrSame::intervalchosen StepSkipOrSame::userinput)	
			(StepSkipOrSame::runtest StepSkipOrSame::num-goes)))))

			
(define (StepSkipOrSame::plus)
  (set! StepSkipOrSame::interval (+ StepSkipOrSame::interval 1))
  (if (> StepSkipOrSame::interval 32)
      (set! StepSkipOrSame::interval 32))
  (d-DirectivePut-score-display "StepSkipOrSame::GameInterval"
				(string-append "<span font_desc=\"12\">Interval: " (object->string StepSkipOrSame::interval) "</span>")))

(define (StepSkipOrSame::minus)
  (set! StepSkipOrSame::interval (- StepSkipOrSame::interval 1))
  (if (< StepSkipOrSame::interval 2)
      (set! StepSkipOrSame::interval 2))
  (d-DirectivePut-score-display "StepSkipOrSame::GameInterval"
				(string-append "<span font_desc=\"12\">Interval: " (object->string StepSkipOrSame::interval) "</span>")))

(define (StepSkipOrSame::setInterval)
  (set! StepSkipOrSame::interval (string->number (d-GetUserInput "Interval" "Give number of steps you want to name note over" (object->string StepSkipOrSame::interval))))
  (if (boolean? StepSkipOrSame::interval)
       (set! StepSkipOrSame::interval 8))
  (if (< StepSkipOrSame::interval 2)
       (set! StepSkipOrSame::interval 2))
  (if (> StepSkipOrSame::interval 32)
      (set! StepSkipOrSame::interval 32))
  (d-DirectivePut-score-display "StepSkipOrSame::GameInterval"
				(string-append "<span font_desc=\"12\">Interval: " (object->string StepSkipOrSame::interval) "</span>")))


(CreateButton "StepSkipOrSame::GameMinus" "<b>-</b>")
(d-SetDirectiveTagActionScript "StepSkipOrSame::GameMinus" "(StepSkipOrSame::minus)")
(CreateButton "StepSkipOrSame::GameInterval" (string-append "<span font_desc=\"12\">Interval: " (object->string StepSkipOrSame::interval) "</span>"))
(d-SetDirectiveTagActionScript "StepSkipOrSame::GameInterval" "(StepSkipOrSame::setInterval)")
(CreateButton "StepSkipOrSame::GamePlus" "<b>+</b>")
(d-SetDirectiveTagActionScript "StepSkipOrSame::GamePlus" "(StepSkipOrSame::plus)")	  

(define (StepSkipOrSame::createbuttons position ustring)
  (CreateButton (string-append "StepSkipOrSame::" position)  (string-append " <span font_desc=\"32\" foreground=\"blue\">" position  "</span>"))
  (d-SetDirectiveTagActionScript  (string-append "StepSkipOrSame::" position) (string-append "(StepSkipOrSame::intervalchosen \"" ustring "\")")))

(StepSkipOrSame::createbuttons "step" "t")
(StepSkipOrSame::createbuttons "skip" "k")
(StepSkipOrSame::createbuttons "same" "a")

(EducationGames::Chime)

(EducationGames::shiftup 6)
(d-Insert2)
(StepSkipOrSame::offerNote)
(set! StepSkipOrSame::start (current-time))
(StepSkipOrSame::runtest StepSkipOrSame::num-goes)
(if (not                                                                                                                                           (EducationGames::Write_Scoreboard_File
  (EducationGames::ScoreboardFile "StepSkipOrSame") StepSkipOrSame::score))
  (StepSkipOrSame::GameOver)
  (StepSkipOrSame::ScoreBoard)
  )

(EducationGames::Chime)

;;;;;;;;;;;;;;;;;;;;;;;;;;

