(define IdentifyScaleNote::Scale (cons "F# G# A# B C#" "fis' gis' ais' b' cis''"))
(define IdentifyScaleNote::notelist '())
(define IdentifyScaleNote::buttonlist '())
(define IdentifyScaleNote::ArpTimer 0)
(define IdentifyScaleNote::score 0)
(define IdentifyScaleNote::CurrentNote "")
(define IdentifyScaleNote::CurrentNoteNum 0)
(define IdentifyScaleNote::NoteIndex 0)

(let ((time (gettimeofday)))
  (set! *random-state*
    (seed->random-state (+ (car time)
      (cdr time)))))

(define (IdentifyScaleNote::gotoEnd)
  (d-CursorRight)
  (if (d-NextObject)
    (IdentifyScaleNote::gotoEnd)
    (d-CursorRight)))

(define (IdentifyScaleNote::lilyname->midikey lilyname)
  (let (
                (naturual_notenum '(0 2 4 5 7 9 11))
                (accidental 0)
                (octave 48)
                (notename 0)
                (notenum 0)
                (loop 0))
      (set! notename
        (lambda (char)
          (modulo (- (char->integer char) 99) 7)))
      (set! loop
        (lambda (x)
          (if (< x (string-length lilyname))
            (begin
              (if (= x 0) (set! notename (notename (string-ref lilyname x))))
              (if (> x 0)
                (begin
                  (if (equal? #\i (string-ref lilyname x))
                    (set! accidental (+ accidental 1)))
                  (if (equal? #\e (string-ref lilyname x))
                    (set! accidental (- accidental 1)))
                  (if (equal? #\' (string-ref lilyname x))
                    (set! octave (+ octave 12)))
                  (if (equal? #\, (string-ref lilyname x))
                    (set! octave (- octave 12)))))
                (loop (+ 1 x)))
                          );end of if
                      )
                    );end of loop
              (loop 0)
    (set! notenum (list-ref naturual_notenum notename))
    (+ (+ octave notenum) accidental)
            );end of let
          )

(define (IdentifyScaleNote::NewNote)
   (set! IdentifyScaleNote::CurrentNoteNum (random (length IdentifyScaleNote::notelist)))
  (set! IdentifyScaleNote::CurrentNote (list-ref IdentifyScaleNote::notelist IdentifyScaleNote::CurrentNoteNum))
  )

(define (IdentifyScaleNote::showscore)
 (d-DirectivePut-score-display "IdentifyScaleNote::GameScore" (string-append "<b>Score: </b>" (number->string IdentifyScaleNote::score))))

(define (IdentifyScaleNote::PlayScaleNote note)
  (let ( (newnote "") )
    (set! newnote (number->string (IdentifyScaleNote::lilyname->midikey note)))
  (d-OneShotTimer IdentifyScaleNote::ArpTimer (string-append "(PlayNote " "\"" newnote "\"" " 1000)"))
  (set! IdentifyScaleNote::ArpTimer (+ IdentifyScaleNote::ArpTimer 1000))))

(define (IdentifyScaleNote::PlayScaleNoteNow note)
  (let ( (newnote "") )
    (set! newnote (number->string (IdentifyScaleNote::lilyname->midikey note)))
  (d-OneShotTimer 0 (string-append "(PlayNote " "\"" newnote "\"" " 1000)"))
  			))

(define (IdentifyScaleNote::PlayScale)
  (set! IdentifyScaleNote::ArpTimer 0)
  (map IdentifyScaleNote::PlayScaleNote IdentifyScaleNote::notelist)
 )

(define (IdentifyScaleNote::DrawNote)
  (d-CursorToNote IdentifyScaleNote::CurrentNote)
  (d-Insert0)
  (d-ChangeChordNotes IdentifyScaleNote::CurrentNote)
)

;TODO perhaps inherit this from EducationGames
(define (IdentifyScaleNote::PlaceAnswerStatus gfx)
  (begin
    (d-DirectivePut-note-minpixels "IdentifyScaleNote::tick" 30)
    (d-DirectivePut-note-gx "IdentifyScaleNote::tick" -10)
    (d-DirectivePut-note-gy "IdentifyScaleNote::tick" 40)
    (d-DirectivePut-note-graphic "IdentifyScaleNote::tick" gfx)))

(define (IdentifyScaleNote::OfferChord)
  (IdentifyScaleNote::showscore)
  (IdentifyScaleNote::NewNote)
  (set! IdentifyScaleNote::ArpTimer 0)
  (IdentifyScaleNote::PlayScaleNote IdentifyScaleNote::CurrentNote)
)

(define (IdentifyScaleNote::Go)
  (IdentifyScaleNote::showscore)
  (IdentifyScaleNote::PlayScale)
  (IdentifyScaleNote::NewNote)
  (set! IdentifyScaleNote::ArpTimer (+ IdentifyScaleNote::ArpTimer 1000))
  (IdentifyScaleNote::PlayScaleNote IdentifyScaleNote::CurrentNote)
)

(define (IdentifyScaleNote::notechosen NoteListPosition)
  (IdentifyScaleNote::DrawNote)
  (IdentifyScaleNote::gotoEnd)
  (if  (= IdentifyScaleNote::CurrentNoteNum (string->number NoteListPosition))
    (begin
      (set! IdentifyScaleNote::score (+ IdentifyScaleNote::score 1))
      (IdentifyScaleNote::PlaceAnswerStatus "CheckMark")
      )
    (begin
      (set! IdentifyScaleNote::score (- IdentifyScaleNote::score 1))
      (IdentifyScaleNote::PlaceAnswerStatus "CrossSign")
  	))
  (d-OneShotTimer 1000  "(IdentifyScaleNote::OfferChord)")
  )

(define (IdentifyScaleNote::createbutton note)
  (CreateButton (string-append "IdentifyScaleNote::" note)  (string-append " <span font_desc=\"22\" foreground=\"blue\">" note  "</span>"))
     (d-SetDirectiveTagActionScript  (string-append "IdentifyScaleNote::" note) (string-append "(IdentifyScaleNote::notechosen \"" (number->string IdentifyScaleNote::NoteIndex) "\")"))
     (set! IdentifyScaleNote::NoteIndex (+ IdentifyScaleNote::NoteIndex 1))
    )
  
(define (IdentifyScaleNote::help)
  (d-InfoDialog "After listening to the scale, there will be a random scale note played. You are determine which note was sounded.")
)
  
;;;;main procedure to call for IdentifyScaleNote
(define (IdentifyScaleNote::IdentifyScaleNotes Scale) 

  (set! IdentifyScaleNote::Scale Scale)
 
  (set! IdentifyScaleNote::buttonlist (string-split (car IdentifyScaleNote::Scale) #\space))
  (set! IdentifyScaleNote::notelist (string-split (cdr IdentifyScaleNote::Scale) #\space))

  (CreateButton "IdentifyScaleNote::GameScore" "<span font_desc=\"22\">Click to start</span>")
  (d-SetDirectiveTagActionScript "IdentifyScaleNote::GameScore" "(IdentifyScaleNote::Go)")

  (CreateButton "IdentifyScaleNote::GameHelp" "<b>Help</b>")
  (d-SetDirectiveTagActionScript "IdentifyScaleNote::GameHelp" "(IdentifyScaleNote::help)")

  (map IdentifyScaleNote::createbutton IdentifyScaleNote::buttonlist)

  (CreateButton "IdentifyScaleNote::replay" "<span font_desc=\"22\">Re-Play</span>")
  (d-SetDirectiveTagActionScript "IdentifyScaleNote::replay" "(IdentifyScaleNote::PlayScaleNoteNow IdentifyScaleNote::CurrentNote)" )

  (CreateButton "IdentifyScaleNote::play_scale" "<span font_desc=\"22\">Play Scale</span>")
  (d-SetDirectiveTagActionScript "IdentifyScaleNote::play_scale" "(IdentifyScaleNote::PlayScale)" )
)
