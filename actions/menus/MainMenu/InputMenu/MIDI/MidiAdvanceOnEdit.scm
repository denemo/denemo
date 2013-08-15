;;;;;;;;;;
;;;MidiAdvance on Edit
(d-Note)
(d-EditMode)
(define-once MidiAdvanceOnEdit::Active #f)
(if MidiAdvanceOnEdit::Active
    (begin
      (d-PutMidi 0)
      (set! MidiAdvanceOnEdit::Active #f))
;;; not active
    (let ((midi 0)
	  (command 0)
	  (velocity 0)
	  (loop 0))
      (begin
	(set! MidiAdvanceOnEdit::Active #t)
	(d-PlayMidiKey #x203001)
	(d-PlayMidiKey #x203401)
	(d-PlayMidiKey #x203701)
	(d-InputFilterNames "Advance on Edit MIDI Filter")
	(set! loop  (lambda ()
			(begin
			  (set! midi (d-GetMidi))
			  (set! velocity (bit-extract midi 16 24))
			  (set! command (bit-extract midi 0 8))
			  
			  (if  (or (= command #x90) (= command #x80))
			       (begin 
				 (if  (= command #x90)
				    (begin
				 (d-PutMidi midi)
				 )
				(begin
					(d-NextNote)
					(d-RefreshDisplay))

							)
				(loop))))))
	(loop))))
