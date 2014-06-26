;ChangeNoteEnharmonic Down
;; This script switches through enharmonic variants of the same in the direction of lower diatonic base notes

(if (Note?)
(if (not (Chord?))
	(d-ChangeEnharmonicDown)
	(let ((target (d-GetNoteAtCursor)))
	    (define old (ANS::GetChordNotes))
	    (define (proc ansNote thetarget)
		(if (eq? thetarget ansNote)
		    (cond 
		        ((= (ANS::GetHalfToneDistanceFromC ansNote) (ANS::GetHalfToneDistanceFromC (- ansNote 40 ))) (- ansNote 40 )) ; special rule for irregular notes
		        ((= (ANS::GetHalfToneDistanceFromC ansNote) (ANS::GetHalfToneDistanceFromC (- ansNote 30 ))) (- ansNote 30)) ; all other notes 
		        (else ansNote))
		    ansNote)) ; else just stay on the same note
	    ;body
	    (if target
		(set! target (ANS::Ly2Ans (string->symbol target))))
	    (ANS::ChangeChordNotes (map (lambda (x) (proc x target)) old)))))
