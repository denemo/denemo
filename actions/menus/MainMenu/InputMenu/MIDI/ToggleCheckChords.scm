;;;ToggleCheckChords

(define-once ToggleCheckChords::Active #f)

(if (not ToggleCheckChords::Active)
  (let ( (PedalDown #f))

    (define (GetNoteOn)
      (let* ( (midi (d-GetMidi))
	  (velocity (bit-extract midi 16 24))
	  (note (bit-extract midi 8 16))
	  (command (bit-extract midi 0 8)))
      (cond
	      ((and (= command #xB0) (= note #x40) (= velocity #x7F));;PedalDown
		(set! PedalDown #t)
		(disp "Short Octave Enabled")
		(GetNoteOn))
	      ((and (= command #xB0) (= note #x40) (= velocity 0));;PedalUp
		(set! PedalDown #f)
		(disp "Split Sharp Enabled")
		(GetNoteOn))
		
	      ((= command #x90)
		(if (and PedalDown (= note 40))
		  (set! note 36))
		(if (and PedalDown (= note 42))
		  (set! note 38))
		(if (and PedalDown (= note 44))
		  (set! note 40))
	      note)
	      ((= command #x80)
	      (GetNoteOn))
	      (else #f))))
    
    (define (burp)
    (PlayNote "32" 500 "127")
      (PlayNote "31" 500 "127"));!!!!!!!!! what if there is no such note on the current chan 0 instrument???
    
    (define (move-cursor thetime)
      (define current (d-GetMidiOnTime))
      (if current
	(if (< current thetime)
	  (if (d-NextNote)
	      (move-cursor thetime)))
	(if (d-NextNote)
	    (move-cursor thetime))))
	    
    (define start (d-GetMidiOnTime))

;;;;;;;; start of the code
    (set! ToggleCheckChords::Active #t)
    (if start
      (set! start (- (d-GetMidiOnTime) 0.0001))
      (set! start 0.0));;;FIXME warn or seek next note
    (d-InputFilterNames (_ "Checking Chords Filter"))

    (d-SetBackground #xD0E0E0)
    (d-RewindMidi start)
    (let nextchord ()
	    (define InitialNotes (d-NextMidiNotes 0.001))
	     (define Notes #f)
	    (define thetime #f)
	    (set! thetime (cdr InitialNotes))
	    (set! Notes (car InitialNotes))
	    (move-cursor thetime)
	    (d-RefreshDisplay)
	    (if (not (null? Notes))	  
	      (let nextnote ()
		(define note (GetNoteOn))
		(if note
		  (if (member note Notes)
		    (begin
		      (PlayNote (number->string note) 500 "127")
		      (set! Notes (delq note Notes))
		      (if (null? Notes)
			  (nextchord)
			  (nextnote)))
		    (begin
		      (disp "Note played was " note " Notes awaited were " Notes "\n")
		      (set! Notes (car InitialNotes))
		      (disp "Replay " Notes " please\n")
		      (burp)
		      (nextnote)
		      ))))))))
    
(d-InputFilterNames (_ "No active MIDI Filter"))
(set! ToggleCheckChords::Active #f)
(d-PutMidi 0);;; to swallow up the last d-GetMidi??
(d-SetMidiCapture #f)	
(d-SetBackground #xFFFFFF)
