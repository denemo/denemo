(define IntervalComparison::Major (cons "Major" "c e g"))
(define IntervalComparison::Minor (cons "Minor" "c ees g"))
(define IntervalComparison::ChordPossibilities (list IntervalComparison::Major IntervalComparison::Minor))

(define IntervalComparison::HighestNote 80)
(define IntervalComparison::LowestNote 55)
(define IntervalComparison::ChordIntervalComparison::LowestNote 60)
(define IntervalComparison::ChordQuality 0)
(define IntervalComparison::ArpTimer 0)
(define TransposedChordNotes '())
(define IntervalComparison::score 0)
(define IntervalComparison::scorepossible 0)





(let ((time (gettimeofday)))
  (set! *random-state*
    (seed->random-state (+ (car time)
      (cdr time)))))

(define (IntervalComparison::gotoEnd)
  (d-CursorRight)
  (if (d-NextObject)
    (IntervalComparison::gotoEnd) 
    (d-CursorRight)))

(define (IntervalComparison::gotoLastObject)
  (d-CursorRight)
  (if (d-NextObject)
    (IntervalComparison::gotoLastObject)))

(define (IntervalComparison::lilyname->midikey lilyname)
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


(define (IntervalComparison::midinum->lilyname num)
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

(define (IntervalComparison::showscore)
 (d-DirectivePut-score-display "IntervalComparison::GameScore" (string-append "<b>Score: </b>" (number->string IntervalComparison::score) "/" (number->string IntervalComparison::scorepossible) )))

(define (IntervalComparison::GetRandom)
  (set! IntervalComparison::ChordIntervalComparison::LowestNote (random IntervalComparison::HighestNote))
  (if (> IntervalComparison::LowestNote IntervalComparison::ChordIntervalComparison::LowestNote)
    (IntervalComparison::GetRandom) ))

(define (IntervalComparison::GetChordQuality)
  (car (list-ref IntervalComparison::ChordPossibilities IntervalComparison::ChordQuality)))
  
(define (IntervalComparison::GetChordSpelling)
  (cdr (list-ref IntervalComparison::ChordPossibilities IntervalComparison::ChordQuality)))

(define (IntervalComparison::GetNewChord)
  (IntervalComparison::GetRandom)  
  (set! IntervalComparison::ChordQuality (random (length IntervalComparison::ChordPossibilities))))

(define (IntervalComparison::GetIntervalList)
 (let (
 	(SetOctave 0)
	(IntervalList 0)
	(ChordNoteList '())
 	)
(set! SetOctave 
  (lambda (lilystring)
    (- (IntervalComparison::lilyname->midikey lilystring) 48)))
  (set! ChordNoteList (string-split (IntervalComparison::GetChordSpelling) #\space))
  (set! IntervalList (map SetOctave ChordNoteList))
  IntervalList
  ))

(define (IntervalComparison::PlayChord note)
  (PlayNote (number->string (+ IntervalComparison::ChordIntervalComparison::LowestNote note)) 1000))

(define (IntervalComparison::Play)  
  (map IntervalComparison::PlayChord (IntervalComparison::GetIntervalList)))

(define (IntervalComparison::ArpegChord note)
  (let ( (newnote "") )
    (set! newnote (number->string (+ IntervalComparison::ChordIntervalComparison::LowestNote note)))
    (d-OneShotTimer IntervalComparison::ArpTimer (string-append "(PlayNote " "\"" newnote "\"" " 1000)"))
    )
  (set! IntervalComparison::ArpTimer (+ IntervalComparison::ArpTimer 1000)))

(define (IntervalComparison::PlayArpeggio)
  (map IntervalComparison::ArpegChord (IntervalComparison::GetIntervalList))
  (set! IntervalComparison::ArpTimer 0))

(define (IntervalComparison::OfferChord)
  (IntervalComparison::showscore)
  (IntervalComparison::GetNewChord)
  (usleep 10000)
  (IntervalComparison::Play))


;;; Initialize Transpose routines.
(define-once Transpose::init #f)
;;(if (not Transpose::init)
;;    (begin
;;;; public variables
      (define Transpose::SetTransposeInterval 0)
      (define Transpose::TransposeNote 0)
      (define Transpose::TransposeNoteList 0)
      (define Transpose::Note "b,")
      (define Transpose::Interval "c b,")

;;;;;;;;;;; private variables
					;original note
      (define Transpose::original-pitch '(0 0 0))
					;transposition amount
      (define Transpose::original-delta '(0 0 0))

      (define Transpose::transpose-origin '(0 0 0))
      (define Transpose::transpose-delta '(0 0 0))
;;;;;;;;;; code

      (define (Transpose::SetTransposeInterval note)
	    (begin
	      (set! Transpose::transpose-delta (Transpose::lilyname->pitch note)) 
	      (Transpose::get-delta)))

      (define (Transpose::get-interval-from-selection)
	(let ((first-note "")
	      (second-note ""))
	  (set! first-note (d-GetNote))
	  (NextChordInSelection)
	  (set! second-note (d-GetNote))
	  (string-append first-note " " second-note)
	  ))

      (define (Transpose::GetTransposeInterval)
	(d-GetUserInput "Setting a Transposition Interval" 
	   "Enter a note, a space, and the note you wish this to transpose to" "c d"))

      (define Transpose::pitch->lilyname
        (lambda (pitch)
  	  (let ((octave->text 0)(accidental->text 0)(pitch->text 0))
    	    (begin
              (set! octave->text
                (lambda (octave_num)
                  (let ((octave_string "")
                        (apply_octave 0))
                    (set! apply_octave
                      (lambda (string value)
                        (begin
                          (if (< value 0)
                            (begin
                              (set! octave_string (string-append octave_string ","))
                              (apply_octave string (+ value 1))))
                          (if (> value 0)
                            (begin
                              (set! octave_string (string-append octave_string "'"))
                              (apply_octave string (- value 1))))
                        )))
                        (apply_octave octave_string octave_num)
                 	octave_string
           	   )))

		(set! accidental->text
		  (lambda (accidental_num)
		  (let ((accidental_string "")
			(apply_accidental 0))
		    (set! apply_accidental
		      (lambda (string value)
			(begin
			  (if (> value 0)
			    (begin
			      (set! accidental_string (string-append accidental_string "is"))
			      (apply_accidental string (- value 1))))
			   (if (< value 0)
			    (begin
			      (set! accidental_string (string-append accidental_string "es"))
			      (apply_accidental string (+ value 1))))
			 )))
		         (apply_accidental accidental_string accidental_num)
			 accidental_string
		   )))

		(set! pitch->text
		  (lambda (pitch_num)
		   (let ((pitch_string ""))
		    (set! pitch_string (integer->char (+ (modulo (+ pitch_num 2) 7) 97)))
		    (string pitch_string)
		    )))
		
		(string-append
		  (pitch->text (cadr pitch))
		  (accidental->text (caddr pitch))
		  (octave->text (car pitch)))

	     )
	     )))

      (define Transpose::lilyname->pitch
	(lambda (lilyname)
	  (let ((accidental 0) (octave 0) (notename 0) (loop 0))
	    (begin
	      (set! notename 
		    (lambda (char)
		      (modulo (- (char->integer char) 99) 7)
		      ))
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
				      (set! octave (+ octave 1)))
				  (if (equal? #\, (string-ref lilyname x))
				      (set! octave (- octave 1)))))
			    (loop (+ 1 x))
			    )
			  );end of if
		      )
		    );end of loop
	      (loop 0)
	      )
	    `(,octave ,notename ,accidental)

	    );end of let
	  ))
      ;;;;copied from chord-name.scm in lilypond-1.6.5

      (define Transpose::semitone-vec (list->vector '(0 2 4 5 7 9 11)))

      (define Transpose::semitone 
	(lambda (pitch)
	  (+ (* (car pitch) 12)
	     (vector-ref Transpose::semitone-vec (modulo (cadr pitch) 7))
	     (caddr pitch))))

      (define Transpose::transpose 
	(lambda (pitch delta)
	  (let ((simple-octave (+ (car pitch) (car delta)))
		(simple-notename (+ (cadr pitch) (cadr delta))))
	    (let ((octave (+ simple-octave (quotient simple-notename 7)))
		  (notename (modulo simple-notename 7)))
	      (let ((accidental (- (+ (Transpose::semitone pitch) (Transpose::semitone delta))
				   (Transpose::semitone `(,octave ,notename 0)))))
		`(,octave ,notename ,accidental))))))

      (define Transpose::get-delta
	(lambda ()
	  (begin
	    (set! Transpose::original-delta (Transpose::transpose Transpose::transpose-origin Transpose::transpose-delta)))))
      
      ;check to see if this is really needed
      (define Transpose::transposed 
        (lambda ()
	  (begin
	    (Transpose::transpose Transpose::original-pitch 
			      Transpose::original-delta ))))

      ;This is in use by the Edit->Transpose->Transpose Selection script.
      (define Transpose::TransposeNote 
        (lambda ()
	  (let ( (numofnotes 0) 
	  	 (notelist '())
		 (eachnote 0)
		 (process_notelist 0)
		 (transposed_notelist ""))

	    (begin
	      (set! process_notelist
		(lambda (note)
		  (set! Transpose::original-pitch (Transpose::lilyname->pitch note))
		        (set! transposed_notelist 
		        (string-append transposed_notelist (Transpose::pitch->lilyname(Transpose::transposed))))
			(set! transposed_notelist (string-append transposed_notelist " "))
		      ))
	      (set! notelist (d-GetNotes))
	      (if (string? notelist)
		  (set! notelist (string-split (d-GetNotes) #\space))
		  (set! notelist '()))
	      (set! numofnotes (length notelist))
	      ;(display "numofnotes = ")
	      ;(display numofnotes)
	      ;(newline)
	      (if (= numofnotes 1)
	        (begin
	    	  (set! Transpose::original-pitch (Transpose::lilyname->pitch (d-GetNotes)))
	          (d-ChangeChordNotes (Transpose::pitch->lilyname(Transpose::transposed)))))	  
	      (if (> numofnotes 1)
	        (begin
		  (map process_notelist notelist)
		  (d-ChangeChordNotes transposed_notelist)
		  )) 
	    ))))
     
      (define Transpose::TransposeNoteList 
        (lambda (string_of_notes)
	  (let ( (numofnotes 0) 
	  	 (outputlist '())
		 (eachnote 0)
		 (process_notelist 0)
		 (transposed_notelist ""))

	    (begin
	      (set! process_notelist
		(lambda (note)
		  (set! Transpose::original-pitch (Transpose::lilyname->pitch note))
		        (set! transposed_notelist 
		        (string-append transposed_notelist (Transpose::pitch->lilyname(Transpose::transposed))))
			(set! transposed_notelist (string-append transposed_notelist " "))
		      ))

	      (set! outputlist (string-split string_of_notes #\space))
	      (set! numofnotes (length outputlist))
	      (display "numofnotes = ")
	      (display numofnotes)
	      (newline)
	      (if (= numofnotes 1)
	        (begin
	    	  (set! Transpose::original-pitch (Transpose::lilyname->pitch string_of_notes))
	          (set! transposed_notelist (string-append (Transpose::pitch->lilyname(Transpose::transposed))))
		  ))	  
	      (if (> numofnotes 1)
		  (map process_notelist outputlist))
		  transposed_notelist
	    ))))

(define Transpose::init #t)
;))



(define (IntervalComparison::TransposeChord notestring lilyname)
  (set! Transpose::Note lilyname)
    (Transpose::SetTransposeInterval Transpose::Note)
  (set! TransposedChordNotes (string-split (Transpose::TransposeNoteList notestring) #\space))
  )
  ;(set! TransposedChordNotes (Transpose::TransposeNoteList notestring)))

(define (IntervalComparison::AddNoteToChord notes)
  (IntervalComparison::gotoLastObject)
  (d-ChangeChordNotes notes))

(define (IntervalComparison::DrawAnimatedArpeggio)
  (let ( (addnotes 0)
         (tindex 0)
         (currentnotes "")
       )
  (set! addnotes
    (lambda (note)
      (begin
        (set! currentnotes (string-append currentnotes " " note))
        (d-OneShotTimer tindex (string-append "(IntervalComparison::AddNoteToChord " "\"" currentnotes "\"" ")"))
	(if (not (string=? note ""))
          (d-OneShotTimer tindex (string-append "(PlayNote " "\"" (number->string (IntervalComparison::lilyname->midikey note)) "\"" " 1000)")))
        (set! tindex (+ tindex 1000))
	            )))
  (set! tindex 0)
  (IntervalComparison::gotoEnd)
  (d-CursorToNote (list-ref TransposedChordNotes 0))
  (d-ToggleImmediatePlayback)
  (d-Insert0)
  (d-ToggleImmediatePlayback)
  (map addnotes TransposedChordNotes)
				))

;TODO perhaps inherit this from EducationGames
(define (IntervalComparison::PlaceAnswerStatus gfx)
  (begin
    (d-DirectivePut-note-minpixels "IntervalComparison::tick" 30)
    (d-DirectivePut-note-gx "IntervalComparison::tick" -10)
    (d-DirectivePut-note-gy "IntervalComparison::tick" 40)
    (d-DirectivePut-note-graphic "IntervalComparison::tick" gfx)))

;;;;;;;;; callback when user chooses a chord
(define (IntervalComparison::chordchosen chord)
  (IntervalComparison::TransposeChord (IntervalComparison::GetChordSpelling)
    (IntervalComparison::midinum->lilyname IntervalComparison::ChordIntervalComparison::LowestNote))
  (IntervalComparison::DrawAnimatedArpeggio)
  (IntervalComparison::gotoEnd)
  (if  (string=? (IntervalComparison::GetChordQuality) chord)
    (begin
      (set! IntervalComparison::score (+ IntervalComparison::score 1))
      (set! IntervalComparison::scorepossible (+ IntervalComparison::scorepossible 1))
      (IntervalComparison::PlaceAnswerStatus "CheckMark")
      )
    (begin
      (set! IntervalComparison::scorepossible (+ IntervalComparison::scorepossible 1))
      (IntervalComparison::PlaceAnswerStatus "CrossSign")
  ))
  (d-OneShotTimer (* 1000 (length TransposedChordNotes)) "(IntervalComparison::OfferChord)")
  )

(define (IntervalComparison::createbuttons chord)
  (CreateButton (string-append "IntervalComparison::" (car chord))  (string-append " <span font_desc=\"18\" foreground=\"blue\">" (car chord)  "</span>"))
    (d-SetDirectiveTagActionScript  (string-append "IntervalComparison::" (car chord)) (string-append "(IntervalComparison::chordchosen \"" (car chord) "\")")))

;;;;main procedure to call for IntervalComparison
(define (IntervalComparison::IntervalComparison chordlist) 

  (set! IntervalComparison::ChordPossibilities chordlist)
  (CreateButton "IntervalComparison::GameScore" "<span font_desc=\"18\">Click to start</span>")
  (d-SetDirectiveTagActionScript "IntervalComparison::GameScore" "(IntervalComparison::OfferChord)")

  (map IntervalComparison::createbuttons IntervalComparison::ChordPossibilities)

  (CreateButton "IntervalComparison::replay" "<span font_desc=\"18\">Re-Play</span>")
  (d-SetDirectiveTagActionScript "IntervalComparison::replay" "(IntervalComparison::Play)" )

  (CreateButton "IntervalComparison::play_arpeggio" "<span font_desc=\"18\">Arpeggio</span>")
  (d-SetDirectiveTagActionScript "IntervalComparison::play_arpeggio" "(IntervalComparison::PlayArpeggio)" )
)



