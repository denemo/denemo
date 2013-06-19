;enharmonic -
;; This script switches through enharmonic variants of the same sounding note in the direction of lower diatonic base notes
(if (Note?)
(let ()
	(define old (ANS::GetChordNotes))
	(define (proc ansNote)
		(cond 
			((= (ANS::GetHalfToneDistanceFromC ansNote) (ANS::GetHalfToneDistanceFromC (-  ansNote 40 ))) (- ansNote 40 )) ; special rule for irregular notes
			((= (ANS::GetHalfToneDistanceFromC ansNote) (ANS::GetHalfToneDistanceFromC (-  ansNote 30 ))) (- ansNote 30)) ; all other notes 
			(else ansNote))) ; else just stay on the same note
	;body
	(ANS::ChangeChordNotes (map (lambda (x) (proc x)) old))))