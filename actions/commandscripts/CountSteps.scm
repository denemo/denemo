;;;;;;;;;;;;;;;
;;CountSteps
;; tests interval recognition.
(define CountSteps::score 0)
(define CountSteps::interval 3)
(define CountSteps::start (current-time))
(define CountSteps::end (current-time))
(define CountSteps::num-goes 30)
(define CountSteps::intervalwas #t)
(define CountSteps::acceptable_list (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
(define CountSteps::note_highest 12)
(define CountSteps::note_lowest 0)
(define CountSteps::note_position 6)
(define CountSteps::userinput #t)
(define CountSteps::direction 0)

(define (CountSteps::showscore)
  (d-DirectivePut-score-display "CountSteps::GameScore" (string-append "<b>Score: " (object->string CountSteps::score) "</b> in " (object->string (- CountSteps::end CountSteps::start)) " Secs.")))

(define (CountSteps::help) 
  (d-InfoDialog "Count how many steps it takes to get to the next note. Enter this number on they keyboard..... ")
)

(define (CountSteps::GameOver)
  (d-InfoDialog (string-append "Game Over\n" "Your Score = " (number->string CountSteps::score)))
)

(define (CountSteps::ScoreBoard)
    (d-InfoDialog (EducationGames::Scoreboard_Pretty_Print
          (EducationGames::ScoreboardFile "CountSteps")))
)

(CreateButton "CountSteps::GameScore" "<span font_desc=\"12\">Score</span>")
(d-SetDirectiveTagActionScript "CountSteps::GameScore" "(CountSteps::ScoreBoard)")
(CreateButton "CountSteps::GameHelp" "<b>Help</b>")
(d-SetDirectiveTagActionScript "CountSteps::GameHelp" "(CountSteps::help)")


;;;;;;;; the main function to run the test - just goes to end and places a note at a random height above the cursor, returning the cursor to where it was.
(define (CountSteps::offerNote) 
  (let (
  	(outofrange? 0)
	(seed 0)
	(steps 0)
	(position 0)
	)
	
	(set! outofrange?
	  (lambda ()
	    (or (> position CountSteps::note_highest) 
	        (> CountSteps::note_lowest position)) 
		))
        (set! seed 
	  (lambda ()
	    (set! steps (random CountSteps::interval))
	    (if (= steps 0)
	      (set! steps 1))
	    (if (= (random 2) 0)
	      (set! position (+ position steps))
	      (set! position (- position steps)))
	      (if (outofrange?)
	        (begin
		  (set! position CountSteps::note_position)
	          (seed))
		(begin
		  (set! CountSteps::note_position position)))
	      ))
  (if (>= CountSteps::num-goes 0)
    (begin
    	(set! position CountSteps::note_position)
        (seed)
	(d-MoveToEnd)
	(d-CursorToNote (EducationalGames::middle_c_offset->lily CountSteps::note_position))
	(d-Insert2)
	(set! CountSteps::intervalwas steps)
	   
	(CountSteps::showscore)
	(set! CountSteps::num-goes (-  CountSteps::num-goes 1))
	)) ;if
	))

;;;;;;;;; callback when user chooses a note
(define (CountSteps::intervalchosen interval)
		      (begin
			(set! CountSteps::end (current-time))
			(d-MoveToEnd)
			(if  (= CountSteps::intervalwas interval)
			     (begin
			       (set! CountSteps::score (+ CountSteps::score 1))
			       (EducationGames::PlaceAnswerStatus "CheckMark"))
			     (begin
			       (set! CountSteps::score (- CountSteps::score 1))
			       (EducationGames::PlaceAnswerStatus "CrossSign"))) 
			       (CountSteps::offerNote)
			       ))
	
;;;;;;;; the main function to run the test
(define CountSteps::runtest 
	(lambda (n)
	  (if (>= n 0) (begin
	                (set! CountSteps::userinput (EducationGames::GetAcceptableKeyInput CountSteps::acceptable_list))
			(set! CountSteps::userinput 
                            (string->number CountSteps::userinput))

			(CountSteps::intervalchosen CountSteps::userinput)	
			(CountSteps::runtest CountSteps::num-goes)))))

			
(define (CountSteps::plus)
  (set! CountSteps::interval (+ CountSteps::interval 1))
  (if (> CountSteps::interval 9)
      (set! CountSteps::interval 9))
  (d-DirectivePut-score-display "CountSteps::GameInterval"
				(string-append "<span font_desc=\"12\">Interval: " (object->string CountSteps::interval) "</span>")))

(define (CountSteps::minus)
  (set! CountSteps::interval (- CountSteps::interval 1))
  (if (< CountSteps::interval 2)
      (set! CountSteps::interval 2))
  (d-DirectivePut-score-display "CountSteps::GameInterval"
				(string-append "<span font_desc=\"12\">Interval: " (object->string CountSteps::interval) "</span>")))

(define (CountSteps::setInterval)
  (set! CountSteps::interval (string->number (d-GetUserInput "Interval" "Give number of steps you want to name note over" (object->string CountSteps::interval))))
  (if (boolean? CountSteps::interval)
       (set! CountSteps::interval 8))
  (if (< CountSteps::interval 2)
       (set! CountSteps::interval 2))
  (if (> CountSteps::interval 9)
      (set! CountSteps::interval 9))
  (d-DirectivePut-score-display "CountSteps::GameInterval"
				(string-append "<span font_desc=\"12\">Interval: " (object->string CountSteps::interval) "</span>")))


(CreateButton "CountSteps::GameMinus" "<b>-</b>")
(d-SetDirectiveTagActionScript "CountSteps::GameMinus" "(CountSteps::minus)")
(CreateButton "CountSteps::GameInterval" (string-append "<span font_desc=\"12\">Interval: " (object->string CountSteps::interval) "</span>"))
(d-SetDirectiveTagActionScript "CountSteps::GameInterval" "(CountSteps::setInterval)")
(CreateButton "CountSteps::GamePlus" "<b>+</b>")
(d-SetDirectiveTagActionScript "CountSteps::GamePlus" "(CountSteps::plus)")





	  
(EducationGames::Chime)

(d-CursorToNote (EducationalGames::middle_c_offset->lily CountSteps::note_position))
(d-Insert2)
(CountSteps::offerNote)
(set! CountSteps::start (current-time))
(CountSteps::runtest CountSteps::num-goes)
(if (not
  (EducationGames::Write_Scoreboard_File
  (EducationGames::ScoreboardFile "CountSteps") CountSteps::score))
  (CountSteps::GameOver)
  (CountSteps::ScoreBoard)
  )

(EducationGames::Chime)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;

