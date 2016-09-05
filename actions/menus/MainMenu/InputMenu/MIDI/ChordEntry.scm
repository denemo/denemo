;;;;ChordEntry
(define-once ChordEntry::active #f)
(define-once ChordEntry::ignore_ties (d-GetBooleanPref "ignore_ties"))
(if ChordEntry::active
   (begin ;;;;Filter is already running stop it
      (disp "Turning off Chord without pedal entry\n")
      (d-InputFilterNames (_ "No MIDI Filter"))
      (d-SetPrefs (string-append "<ignore_ties>" (if ChordEntry::ignore_ties "1" "0") "</ignore_ties>"))
      (set! ChordEntry::active #f)
      (d-PutMidi 0)
      (d-SetPrefs "<ignore_ties>0</ignore_ties>")
      (d-SetBackground #xFFFFFF))
   
  (let  ((ons '())) ;;;;Filter is not already running so run it
    (define (noteOn? midi)
      (= #x90 (bit-extract midi 0 8)))
    (define (noteOff? midi)
      (= #x80 (bit-extract midi 0 8)))
    (define (add-note note)
      (d-InsertNoteInChord (d-GetNoteForMidiKey note))  
      (PlayNote (number->string note) 400))
    
    (define (GetChords)
      (let loop ()
        (let* (
               (midi (d-GetMidi))
               (velocity (bit-extract midi 16 24))
               (note (bit-extract midi 8 16))
               (command (bit-extract midi 0 8)))  
          (cond
               ((noteOn? midi)
                    (if (null? ons)
                        (begin
                            (d-PutMidi midi)
                            (if (not (Appending?))
                                (d-PrevChord)))
                        (add-note note))
                    (set! ons (cons note ons))
                    (loop))
               ((noteOff? midi)       
                    (set! ons (delq note ons))
                    (if (null? ons)
                         (d-NextChord))
                    (loop))
               ((or (zero? midi) (and  (= command #xE0) (> note 32)))
                    (d-SetBackground #xFFFFFF)
                    (d-SetPrefs (string-append "<ignore_ties>" (if ChordEntry::ignore_ties "1" "0") "</ignore_ties>"))
                    (disp "Abandoning getting chords\n"))
               (else (DenemoPutMidi midi)(loop)))))) ;;; end of GetChords

    ;;;;;;;; actual code
    (set! ChordEntry::active #t)
    (d-InputFilterNames (_ "Chord entry without pedal"))
    (d-SetBackground #xC0E0E0)
    (set! ChordEntry::ignore_ties (d-GetBooleanPref "ignore_ties"))
    (d-SetPrefs "<ignore_ties>1</ignore_ties>")
    (GetChords)))
