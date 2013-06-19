;;;;;;;;;;;;;;;
;;DiatonicInterval
;; tests interval recognition.
(define DiatonicInterval::score 0)
(define DiatonicInterval::interval 3)
(define DiatonicInterval::acceptable_list (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
(define DiatonicInterval::start (current-time))
(define DiatonicInterval::end (current-time))
(define DiatonicInterval::num-goes 30)
(define DiatonicInterval::intervalwas #t)
(define DiatonicInterval::userinput #t)
(define DiatonicInterval::note_highest 12)
(define DiatonicInterval::note_lowest 0)
(define DiatonicInterval::note_position 6)
(define DiatonicInterval::direction 0)

(define (DiatonicInterval::showscore)
  (d-DirectivePut-score-display "DiatonicInterval::GameScore" (string-append "<b>Score: " (object->string DiatonicInterval::score) "</b> in " (object->string (- DiatonicInterval::end DiatonicInterval::start)) " Secs.")))

(define (DiatonicInterval::help) 
  (d-InfoDialog "Enter the interval value that the note changes. If the note moves by a 2nd hit 2, by a 3rd hit 3 etc..... ")
)

(define (DiatonicInterval::GameOver)
  (d-InfoDialog (string-append "Game Over\n" "Your Score = " (number->string DiatonicInterval::score)))
)

(define (DiatonicInterval::ScoreBoard)
  (d-InfoDialog  (EducationGames::Scoreboard_Pretty_Print
      (EducationGames::ScoreboardFile "DiatonicInterval")))
)

(CreateButton "DiatonicInterval::GameScore" "<span font_desc=\"12\">Score</span>")
(d-SetDirectiveTagActionScript "DiatonicInterval::GameScore" "(DiatonicInterval::ScoreBoard)")
(CreateButton "DiatonicInterval::GameHelp" "<b>Help</b>")
(d-SetDirectiveTagActionScript "DiatonicInterval::GameHelp" "(DiatonicInterval::help)")


;;;;;;;; the main function to run the test - just goes to end and places a note at a random height above the cursor, returning the cursor to where it was.
(define (DiatonicInterval::offerNote) 
  (let (
  	(outofrange? 0)
	(seed 0)
	(steps 0)
	(position 0)
	)
	
	(set! outofrange?
	  (lambda ()
	    (or (> position DiatonicInterval::note_highest) 
	        (> DiatonicInterval::note_lowest position)) 
		))
        (set! seed 
	  (lambda ()
	    (set! steps (random DiatonicInterval::interval))
	    (if (= steps 0)
	      (set! steps 1))
	    (if (= (random 2) 0)
	      (set! position (+ position steps))
	      (set! position (- position steps)))
	      (if (outofrange?)
	        (begin 
                  (set! position DiatonicInterval::note_position)
		  (seed))
		(begin
		  (set! DiatonicInterval::note_position position)))
	      ))
  (if (>= DiatonicInterval::num-goes 0)
    (begin
	(set! position DiatonicInterval::note_position)
	(seed)
	(d-MoveToEnd)
	(d-CursorToNote (EducationalGames::middle_c_offset->lily DiatonicInterval::note_position))
	(d-Insert2)
	(set! DiatonicInterval::intervalwas (+ 1 steps))
	   
	(DiatonicInterval::showscore)
	(set! DiatonicInterval::num-goes (-  DiatonicInterval::num-goes 1))
	)) ;if
	))

;;;;;;;;; callback when user chooses a note
(define (DiatonicInterval::intervalchosen interval)
		      (begin
			(set! DiatonicInterval::end (current-time))
			(d-MoveToEnd)
			(if  (= DiatonicInterval::intervalwas interval)
			     (begin
			       (set! DiatonicInterval::score (+ DiatonicInterval::score 1))
			       (EducationGames::PlaceAnswerStatus "CheckMark"))
			     (begin
			       (set! DiatonicInterval::score (- DiatonicInterval::score 1))
			       (EducationGames::PlaceAnswerStatus "CrossSign"))) 
			       (DiatonicInterval::offerNote)
			       ))
	
;;;;;;;; the main function to run the test
(define DiatonicInterval::runtest 
	(lambda (n)
	  (if (>= n 0) (begin
	                (set! DiatonicInterval::userinput (EducationGames::GetAcceptableKeyInput DiatonicInterval::acceptable_list))
			(set! DiatonicInterval::userinput 
			    (string->number DiatonicInterval::userinput))

			(DiatonicInterval::intervalchosen DiatonicInterval::userinput)	
			(DiatonicInterval::runtest DiatonicInterval::num-goes)))))

			
(define (DiatonicInterval::plus)
  (set! DiatonicInterval::interval (+ DiatonicInterval::interval 1))
  (if (> DiatonicInterval::interval 9)
      (set! DiatonicInterval::interval 9))
  (d-DirectivePut-score-display "DiatonicInterval::GameInterval"
				(string-append "<span font_desc=\"12\">Interval: " (object->string DiatonicInterval::interval) "</span>")))

(define (DiatonicInterval::minus)
  (set! DiatonicInterval::interval (- DiatonicInterval::interval 1))
  (if (< DiatonicInterval::interval 2)
      (set! DiatonicInterval::interval 2))
  (d-DirectivePut-score-display "DiatonicInterval::GameInterval"
				(string-append "<span font_desc=\"12\">Interval: " (object->string DiatonicInterval::interval) "</span>")))

(define (DiatonicInterval::setInterval)
  (set! DiatonicInterval::interval (string->number (d-GetUserInput "Interval" "Give number of steps you want to name note over" (object->string DiatonicInterval::interval))))
  (if (boolean? DiatonicInterval::interval)
       (set! DiatonicInterval::interval 8))
  (if (< DiatonicInterval::interval 2)
       (set! DiatonicInterval::interval 2))
  (if (> DiatonicInterval::interval 9)
      (set! DiatonicInterval::interval 9))
  (d-DirectivePut-score-display "DiatonicInterval::GameInterval"
				(string-append "<span font_desc=\"12\">Interval: " (object->string DiatonicInterval::interval) "</span>")))


(CreateButton "DiatonicInterval::GameMinus" "<b>-</b>")
(d-SetDirectiveTagActionScript "DiatonicInterval::GameMinus" "(DiatonicInterval::minus)")
(CreateButton "DiatonicInterval::GameInterval" (string-append "<span font_desc=\"12\">Interval: " (object->string DiatonicInterval::interval) "</span>"))
(d-SetDirectiveTagActionScript "DiatonicInterval::GameInterval" "(DiatonicInterval::setInterval)")
(CreateButton "DiatonicInterval::GamePlus" "<b>+</b>")
(d-SetDirectiveTagActionScript "DiatonicInterval::GamePlus" "(DiatonicInterval::plus)")

(EducationGames::Chime)

(d-CursorToNote (EducationalGames::middle_c_offset->lily DiatonicInterval::note_position))
(d-Insert2)
(DiatonicInterval::offerNote)
(set! DiatonicInterval::start (current-time))
(DiatonicInterval::runtest DiatonicInterval::num-goes)
(if (not (EducationGames::Write_Scoreboard_File
  (EducationGames::ScoreboardFile "DiatonicInterval") DiatonicInterval::score))
  (DiatonicInterval::GameOver)
  (DiatonicInterval::ScoreBoard)
  )

(EducationGames::Chime)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;

