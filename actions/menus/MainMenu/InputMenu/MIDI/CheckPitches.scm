;;;;;;;;;;
;;;CheckPitches
(d-Note)
(d-EditMode)
(define-once CheckPitches::Active #f)
(if CheckPitches::Active
    (begin
      (d-PutMidi 0)
      (set! CheckPitches::Active #f))
;;; not active
    (let (
	  (midi 0)
	  (command 0)
	  (note 0)
	  (velocity 0)
	  (loop 0))
      (begin
	(set! CheckPitches::Active #t)
	(d-PlayMidiKey #x202001)
	(d-PlayMidiKey #x202601)
	(d-PlayMidiKey #x202201)
        (d-InputFilterNames "Check Pitches Filter")
	(d-SetMidiCapture #t)	
	(set! loop  (lambda ()
			(begin
			  (set! midi (d-GetMidi))
			  (set! velocity (bit-extract midi 16 24))
			  (set! note (bit-extract midi 8 16))
			  (set! command (bit-extract midi 0 8))
                          (if (= command #x80)
				(loop))
			  (if  (= command #x90);; only check on channel 0
			       (begin 				 
				(d-PlayMidiKey midi)
				 
				       (if (= (d-GetNoteAsMidi) note)
				       (begin (d-NextNote)(d-RefreshDisplay))
				       (begin (d-PlayMidiKey #xF06001))
				       )
				       (loop))))))
	(loop))))
(d-SetMidiCapture #f)
(display "End of script")
