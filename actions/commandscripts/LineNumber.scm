(define LineNumber::Scale (cons "1 2 3 4 5" "e' g' b' d'' f''"))
(define LineNumber::notelist '())
(define LineNumber::buttonlist '())
(define LineNumber::score 0)
(define LineNumber::CurrentNote "")
(define LineNumber::CurrentNoteNum 0)
(define LineNumber::NoteIndex 0)

(let ((time (gettimeofday)))
  (set! *random-state*
    (seed->random-state (+ (car time)
      (cdr time)))))

(define (LineNumber::gotoEnd)
  (d-CursorRight)
  (if (d-NextObject)
    (LineNumber::gotoEnd)
    (d-CursorRight)))

(define (LineNumber::NewNote)
  (LineNumber::gotoEnd)
  (set! LineNumber::CurrentNoteNum (random (length LineNumber::notelist)))
  (set! LineNumber::CurrentNote (list-ref LineNumber::notelist LineNumber::CurrentNoteNum))
  (d-CursorToNote LineNumber::CurrentNote)
  (d-Insert0)
  )

(define (LineNumber::showscore)
 (d-DirectivePut-score-display "LineNumber::GameScore" (string-append "<b>Score: </b>" (number->string LineNumber::score))))


;TODO perhaps inherit this from EducationGames
(define (LineNumber::PlaceAnswerStatus gfx)
  (begin
    (d-DirectivePut-note-minpixels "LineNumber::tick" 30)
    (d-DirectivePut-note-gx "LineNumber::tick" -10)
    (d-DirectivePut-note-gy "LineNumber::tick" 40)
    (d-DirectivePut-note-graphic "LineNumber::tick" gfx)))

(define (LineNumber::OfferChord)
  (LineNumber::showscore)
  (LineNumber::NewNote)
)

(define (LineNumber::Go)
  (LineNumber::showscore)
  (LineNumber::gotoEnd)
  (LineNumber::OfferChord)
)

(define (LineNumber::notechosen NoteListPosition)
  (if  (= LineNumber::CurrentNoteNum (string->number NoteListPosition))
    (begin
      (set! LineNumber::score (+ LineNumber::score 1))
      (LineNumber::PlaceAnswerStatus "CheckMark")
      )
    (begin
      (set! LineNumber::score (- LineNumber::score 1))
      (LineNumber::PlaceAnswerStatus "CrossSign")
  	))
   (LineNumber::OfferChord)
  )

(define (LineNumber::createbutton note)
  (CreateButton (string-append "LineNumber::" note)  (string-append " <span font_desc=\"22\" foreground=\"blue\">" note  "</span>"))
     (d-SetDirectiveTagActionScript  (string-append "LineNumber::" note) (string-append "(LineNumber::notechosen \"" (number->string LineNumber::NoteIndex) "\")"))
     (set! LineNumber::NoteIndex (+ LineNumber::NoteIndex 1))
    )
  
(define (LineNumber::help)
  (d-InfoDialog "After you see the note appear on the screen hit the number that corresponds with the line that it is on.")
)
  
;;;;main procedure to call for LineNumber
(define (LineNumber::LineNumbers Scale) 

  (set! LineNumber::Scale Scale)
 
  (set! LineNumber::buttonlist (string-split (car LineNumber::Scale) #\space))
  (set! LineNumber::notelist (string-split (cdr LineNumber::Scale) #\space))

  (CreateButton "LineNumber::GameScore" "<span font_desc=\"22\">Click to start</span>")
  (d-SetDirectiveTagActionScript "LineNumber::GameScore" "(LineNumber::Go)")

  (CreateButton "LineNumber::GameHelp" "<b>Help</b>")
  (d-SetDirectiveTagActionScript "LineNumber::GameHelp" "(LineNumber::help)")

  (map LineNumber::createbutton LineNumber::buttonlist)
)

(LineNumber::LineNumbers LineNumber::Scale) 