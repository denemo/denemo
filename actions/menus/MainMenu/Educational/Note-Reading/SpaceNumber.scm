(define SpaceNumber::Scale (cons "1 2 3 4" "f' a' c'' e''"))
(define SpaceNumber::notelist '())
(define SpaceNumber::buttonlist '())
(define SpaceNumber::score 0)
(define SpaceNumber::CurrentNote "")
(define SpaceNumber::CurrentNoteNum 0)
(define SpaceNumber::NoteIndex 0)

(let ((time (gettimeofday)))
  (set! *random-state*
    (seed->random-state (+ (car time)
      (cdr time)))))

(define (SpaceNumber::gotoEnd)
  (d-CursorRight)
  (if (d-NextObject)
    (SpaceNumber::gotoEnd)
    (d-CursorRight)))

(define (SpaceNumber::NewNote)
  (SpaceNumber::gotoEnd)
  (set! SpaceNumber::CurrentNoteNum (random (length SpaceNumber::notelist)))
  (set! SpaceNumber::CurrentNote (list-ref SpaceNumber::notelist SpaceNumber::CurrentNoteNum))
  (d-CursorToNote SpaceNumber::CurrentNote)
  (d-Insert0)
  )

(define (SpaceNumber::showscore)
 (d-DirectivePut-score-display "SpaceNumber::GameScore" (string-append "<b>Score: </b>" (number->string SpaceNumber::score))))


;TODO perhaps inherit this from EducationGames
(define (SpaceNumber::PlaceAnswerStatus gfx)
  (begin
    (d-DirectivePut-note-minpixels "SpaceNumber::tick" 30)
    (d-DirectivePut-note-gx "SpaceNumber::tick" -10)
    (d-DirectivePut-note-gy "SpaceNumber::tick" 40)
    (d-DirectivePut-note-graphic "SpaceNumber::tick" gfx)))

(define (SpaceNumber::OfferChord)
  (SpaceNumber::showscore)
  (SpaceNumber::NewNote)
)

(define (SpaceNumber::Go)
  (SpaceNumber::showscore)
  (SpaceNumber::gotoEnd)
  (SpaceNumber::OfferChord)
)

(define (SpaceNumber::notechosen NoteListPosition)
  (if  (= SpaceNumber::CurrentNoteNum (string->number NoteListPosition))
    (begin
      (set! SpaceNumber::score (+ SpaceNumber::score 1))
      (SpaceNumber::PlaceAnswerStatus "CheckMark")
      )
    (begin
      (set! SpaceNumber::score (- SpaceNumber::score 1))
      (SpaceNumber::PlaceAnswerStatus "CrossSign")
  	))
   (SpaceNumber::OfferChord)
  )

(define (SpaceNumber::createbutton note)
  (CreateButton (string-append "SpaceNumber::" note)  (string-append " <span font_desc=\"22\" foreground=\"blue\">" note  "</span>"))
     (d-SetDirectiveTagActionScript  (string-append "SpaceNumber::" note) (string-append "(SpaceNumber::notechosen \"" (number->string SpaceNumber::NoteIndex) "\")"))
     (set! SpaceNumber::NoteIndex (+ SpaceNumber::NoteIndex 1))
    )
  
(define (SpaceNumber::help)
  (d-InfoDialog "After you see the note appear on the screen hit the number that corresponds with the line that it is on.")
)
  
;;;;main procedure to call for SpaceNumber
(define (SpaceNumber::SpaceNumbers Scale) 

  (set! SpaceNumber::Scale Scale)
 
  (set! SpaceNumber::buttonlist (string-split (car SpaceNumber::Scale) #\space))
  (set! SpaceNumber::notelist (string-split (cdr SpaceNumber::Scale) #\space))

  (CreateButton "SpaceNumber::GameScore" "<span font_desc=\"22\">Click to start</span>")
  (d-SetDirectiveTagActionScript "SpaceNumber::GameScore" "(SpaceNumber::Go)")

  (CreateButton "SpaceNumber::GameHelp" "<b>Help</b>")
  (d-SetDirectiveTagActionScript "SpaceNumber::GameHelp" "(SpaceNumber::help)")

  (map SpaceNumber::createbutton SpaceNumber::buttonlist)
)

(SpaceNumber::SpaceNumbers SpaceNumber::Scale) 