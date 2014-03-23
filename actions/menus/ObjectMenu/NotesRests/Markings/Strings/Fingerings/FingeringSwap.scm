(let ((finger1 #f)(finger2 #f))
(set! finger1 (d-GetUserInput (_ "Fingering Swap") (_ "Give first fingering") "1"))
(if finger1
	(begin
		(set! finger2 (d-GetUserInput  (_ "Fingering Swap") (_ "Give second fingering") "2"))
		(d-DirectivePut-note-postfix "Fingering" (string-append "\\finger \\markup \\tied-lyric #\"" finger1 "~" finger2  "\" "))
		(d-DirectivePut-note-display"Fingering" (string-append finger1 "~" finger2))))
(d-SetSaved #f)
(d-Chordize #t))
