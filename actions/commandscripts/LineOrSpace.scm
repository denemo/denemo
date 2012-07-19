;;;;;;;;;;;;;;;
;;LineOrSpace
;; tests note name recognition.

(define LineOrSpace::positionwas #t)
(define LineOrSpace::acceptable_input (list "l" "space"))
(define LineOrSpace::userinput #t)
(define LineOrSpace::start (current-time))
(define LineOrSpace::end (current-time))
(define LineOrSpace::score 0)
(define LineOrSpace::steps 0)
(define LineOrSpace::position 6) ;in middle c offset
(define LineOrSpace::note_highest 12)
(define LineOrSpace::note_lowest 0)
(define LineOrSpace::span 8) ;; how many LineOrSpace::steps of the scale to test.
(define LineOrSpace::num-goes 30) ;; how many notes to present for the whole test
(define LineOrSpace::input_device 1) ;0 = mouse 1 = keyboard

(define (LineOrSpace::showscore)
  (d-DirectivePut-score-display "LineOrSpace::GameScore" (string-append "<b>Score: " (object->string LineOrSpace::score) "</b> in " (object->string (- LineOrSpace::end LineOrSpace::start)) " Secs.")))

(define (LineOrSpace::help) 
  (d-InfoDialog "Click on the *Spacebar* if the note is on a space and the *L* key if it is on a line.")
)

(define (LineOrSpace::GameOver)
   (d-InfoDialog (string-append "Game Over\n" "Your Score = " (number->string LineOrSpace::score)))
)

(define (LineOrSpace::ScoreBoard)
    (d-InfoDialog (EducationGames::Scoreboard_Pretty_Print 
      (EducationGames::ScoreboardFile "LineOrSpace")))
)

(CreateButton "LineOrSpace::GameScore" "<span font_desc=\"12\">Score</span>")
(d-SetDirectiveTagActionScript "LineOrSpace::GameScore" "(LineOrSpace::ScoreBoard)")
(CreateButton "LineOrSpace::GameHelp" "<b>Help</b>")
(d-SetDirectiveTagActionScript "LineOrSpace::GameHelp" "(LineOrSpace::help)")
	  
(define (LineOrSpace::offerNote) 
	(let (    
	      (outofrange? 0)
	      (seed 0)
	      (interval 0)
	      )
	      (set! outofrange?
	        (lambda ()
		  (or (> LineOrSpace::position LineOrSpace::note_highest) 
		      (> LineOrSpace::note_lowest LineOrSpace::position)) 
			))
	      (set! seed 
	        (lambda ()
                  (set! interval (random 4))
	          (if (= (random 2) 0)
	            (set! LineOrSpace::position (+ LineOrSpace::position interval))
		    (set! LineOrSpace::position (- LineOrSpace::position interval)))
		    (if (outofrange?)
		      (seed))
		    ))

          (seed)
	  (d-MoveToEnd)
	  (d-CursorToNote (EducationalGames::middle_c_offset->lily LineOrSpace::position))
	  (d-Insert1)
	  (if (or (even? LineOrSpace::position) 
	          (= 0 LineOrSpace::position))
	      (set! LineOrSpace::positionwas "line")
	      (set! LineOrSpace::positionwas "space"))
 
	  (LineOrSpace::showscore)
	  ))

;;;;;;;;; callback when user chooses a note
(define (LineOrSpace::positionchosen userinput)
	 (if (> LineOrSpace::num-goes 0)
		(begin
			(set! LineOrSpace::end (current-time))
			(d-MoveToEnd)
			(if  (string=? LineOrSpace::positionwas userinput)
			     (begin
			       (set! LineOrSpace::score (+ LineOrSpace::score 1))
			       (EducationGames::PlaceAnswerStatus "CheckMark"))
			     (begin
			       (set! LineOrSpace::score (- LineOrSpace::score 1))
			       (EducationGames::PlaceAnswerStatus "CrossSign"))) 
			
			(if (= LineOrSpace::input_device 0)
			        (begin
				  (set! LineOrSpace::num-goes (- LineOrSpace::num-goes 1)) 
				  (if (= LineOrSpace::num-goes 0)
				    (LineOrSpace::EndGame))
				    ))
		        (if (> LineOrSpace::num-goes 0)
			  (LineOrSpace::offerNote))
			       )))
	

;;;;;;;; the main function to run the test
(define LineOrSpace::runtest 
	(lambda (n)
	  (if (> n 0) (begin
	      (set! LineOrSpace::userinput (EducationGames::GetAcceptableKeyInput LineOrSpace::acceptable_input))
	      (if (string=? LineOrSpace::userinput "l")
	           (set! LineOrSpace::userinput "line"))
	      (LineOrSpace::positionchosen LineOrSpace::userinput)	
	      (LineOrSpace::runtest (- n 1))))))


(define (LineOrSpace::createbuttons position)
  (CreateButton (string-append "LineOrSpace::" position)  (string-append " <span font_desc=\"32\" foreground=\"blue\">" position  "</span>"))
  (d-SetDirectiveTagActionScript  (string-append "LineOrSpace::" position) (string-append "(LineOrSpace::positionchosen \"" position "\")")))

(define (LineOrSpace::EndGame)
    (EducationGames::Chime)  
    (if (not 
      (EducationGames::Write_Scoreboard_File 
      (EducationGames::ScoreboardFile "LineOrSpace") LineOrSpace::score))
      (LineOrSpace::GameOver)
      (LineOrSpace::ScoreBoard)
    ))


;;;
(define (LineOrSpace::go)
  (if (not (zero? LineOrSpace::score))
      (let ((response #f))
	(set! response (d-GetUserInput "Reset Score" "Do you want to reset your score" "y"))
	(if (equal? response "y")
	    (begin
	      (set! LineOrSpace::start (current-time))
	      (set! LineOrSpace::end (current-time))

	      (set! LineOrSpace::score 0)))))
  (d-DeletePreviousObject)
  (if (= LineOrSpace::input_device 0)
    (begin ;mouse 
      (LineOrSpace::createbuttons "line")
      (LineOrSpace::createbuttons "space")
      (EducationGames::Chime)
      (LineOrSpace::offerNote)
      )
    (begin ;keyboard
      (EducationGames::Chime)
      (LineOrSpace::offerNote)
      (LineOrSpace::runtest LineOrSpace::num-goes)
      (LineOrSpace::EndGame))
      )
   )

(CreateButton "LineOrSpace::GameScore" "<span font_desc=\"32\">Click to start</span>")
(d-SetDirectiveTagActionScript "LineOrSpace::GameScore" "(LineOrSpace::go)")


(define (LineOrSpace::Input_Select)
  (if (= LineOrSpace::input_device 0)
    (begin 
      (set! LineOrSpace::input_device 1)
      (d-DirectivePut-score-display "LineOrSpace::SetInput" "<span font_desc=\"12\">Input:keyboard</span>")
      )
    (begin
      (set! LineOrSpace::input_device 0)
      (d-DirectivePut-score-display "LineOrSpace::SetInput" "<span font_desc=\"12\">Input:mouse</span>")
    )  
    ))

(CreateButton "LineOrSpace::SetInput" "<span font_desc=\"12\">Input:keyboard</span>")
(d-SetDirectiveTagActionScript "LineOrSpace::SetInput" "(LineOrSpace::Input_Select)")

