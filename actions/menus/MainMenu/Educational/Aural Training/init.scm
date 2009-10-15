
(define Major (cons "Major" "c e g"))
(define Minor (cons "Minor" "c ees g"))
(define Augmented (cons "Augmented" "c e gis"))
(define Diminished (cons "Diminished" "c ees ges"))
(define Major7 (cons "Major7" "c e g b"))
(define Dominant7 (cons "Dominant7" "c e g bes"))
(define Minor7 (cons "Minor7" "c ees g bes"))
(define HalfDiminished7 (cons "HalfDiminished7" "c ees ges bes"))
(define Diminished7 (cons "Diminished7" "c ees ges beses"))


(define ChordComparison::ChordPossibilities (list Major Minor))

(define ChordComparison::HighestNote 80)
(define ChordComparison::LowestNote 55)
(define ChordComparison::ChordChordComparison::LowestNote 60)
(define ChordComparison::ChordQuality 0)
(define ChordComparison::score 0)



(let ((time (gettimeofday)))
  (set! *random-state*
    (seed->random-state (+ (car time)
      (cdr time)))))

(define (lilyname->midikey lilyname)
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


(define (midinum->lilyname num)
  (let ( 	(octave 0) 
  		(notename "")
  		(OctaveString "")
  		(sharplist '("c" "cis" "d" "dis" "e" "f" "fis" "g" "gis" "a" "ais" "b"))
  				)
  (set! octave (- (quotient num 12) 4))
  (set! notename (list-ref sharplist (remainder num 12)))
  (if (> octave 0)
    (set! OctaveString (string-pad "" (abs octave) #\'))
    (set! OctaveString (string-pad "" (abs octave) #\,)))
  (string-append notename OctaveString)
))

(define (ChordComparison::showscore)
 (d-DirectivePut-score-display "ChordComparison::GameScore" (string-append "<b>Score: </b>" (number->string ChordComparison::score))))

(define (ChordComparison::GetRandom)
  (set! ChordComparison::ChordChordComparison::LowestNote (random ChordComparison::HighestNote))
  (if (> ChordComparison::LowestNote ChordComparison::ChordChordComparison::LowestNote)
    (ChordComparison::GetRandom) ))

(define (ChordComparison::GetChordQuality)
  (car (list-ref ChordComparison::ChordPossibilities ChordComparison::ChordQuality)))
  
(define (ChordComparison::GetChordSpelling)
  (cdr (list-ref ChordComparison::ChordPossibilities ChordComparison::ChordQuality)))

(define (ChordComparison::GetNewChord)
  (ChordComparison::GetRandom)  
  (set! ChordComparison::ChordQuality (random (length ChordComparison::ChordPossibilities))))

(define (ChordComparison::GetIntervalList)
 (let (
 	(SetOctave 0)
	(IntervalList 0)
	(ChordNoteList '())
 	)
(set! SetOctave 
  (lambda (lilystring)
    (- (lilyname->midikey lilystring) 48)))
  (set! ChordNoteList (string-split (ChordComparison::GetChordSpelling) #\space))
  (set! IntervalList (map SetOctave ChordNoteList))
  IntervalList
  ))

(define (ChordComparison::PlayChord note)
  (PlayNote (number->string (+ ChordComparison::ChordChordComparison::LowestNote note)) 1000))

(define (ChordComparison::Play)  
  (map ChordComparison::PlayChord (ChordComparison::GetIntervalList)))

(define (ChordComparison::OfferChord)
  (ChordComparison::showscore)
  (ChordComparison::GetNewChord)
  (usleep 10000)
  (ChordComparison::Play))

(if (not (defined? 'Transpose::init))
  (begin
    (d-LoadCommand "/MainMenu/EditMenu/Transpose/SetTransposeIntervalFromNote")
    (d-InitializeScript "SetTransposeIntervalFromNote")))

(define (ChordComparison::TransposeChord lilyname)
  (set! Transpose::Note lilyname)
  (Transpose::SetTransposeInterval Transpose::Note)
  (Transpose::TransposeNote))

(define (ChordComparison::PlaceNotes)
  (d-CursorToNote "c")
  (d-Insert2)
  (d-ChangeChordNotes (ChordComparison::GetChordSpelling))
  (ChordComparison::TransposeChord (midinum->lilyname ChordComparison::ChordChordComparison::LowestNote)))

;TODO perhaps inherit this from EducationGames
(define (ChordComparison::PlaceAnswerStatus gfx)
  (begin
    (d-DirectivePut-note-minpixels "ChordComparison::tick" 30)
    (d-DirectivePut-note-gx "ChordComparison::tick" -15)
    (d-DirectivePut-note-gy "ChordComparison::tick" 40)
    (d-DirectivePut-note-graphic "ChordComparison::tick" gfx)))

;;;;;;;;; callback when user chooses a chord
(define (ChordComparison::chordchosen chord)
  (ChordComparison::PlaceNotes) 
  (let gotoEnd () (if  (d-NextObject) (gotoEnd)))
  (if  (string=? (ChordComparison::GetChordQuality) chord)
    (begin
      (set! ChordComparison::score (+ ChordComparison::score 1))
      (ChordComparison::PlaceAnswerStatus "CheckMark")
      )
    (begin
      (set! ChordComparison::score (- ChordComparison::score 1))
      (ChordComparison::PlaceAnswerStatus "CrossSign")
  ))
  (ChordComparison::OfferChord))

(define (ChordComparison::createbuttons chord)
  (CreateButton (string-append "ChordComparison::" (car chord))  (string-append " <span font_desc=\"32\" foreground=\"blue\">" (car chord)  "</span>"))
    (d-SetDirectiveTagActionScript  (string-append "ChordComparison::" (car chord)) (string-append "(ChordComparison::chordchosen \"" (car chord) "\")")))

;;;;main procedure to call for ChordComparison
(define (ChordComparison::ChordComparison chordlist) 

  (set! ChordComparison::ChordPossibilities chordlist)
  (CreateButton "ChordComparison::GameScore" "<span font_desc=\"32\">Click to start</span>")
  (d-SetDirectiveTagActionScript "ChordComparison::GameScore" "(ChordComparison::OfferChord)")

  (map ChordComparison::createbuttons ChordComparison::ChordPossibilities)

  (CreateButton "ChordComparison::replay" "<span font_desc=\"32\">Re-Play</span>")
  (d-SetDirectiveTagActionScript "ChordComparison::replay" "(ChordComparison::Play)" )
)


