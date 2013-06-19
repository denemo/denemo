;;;CreatePartsFromTop
(let ()

(define (delete-highest-notes)
  ;delete highest note of each chord
  (d-MoveToBeginning)
  (let loop () 
    (if (d-NextChord) 
	(begin 
	  (if (d-CursorToNote (d-GetNoteFromTop)) (d-StagedDelete))
	  (loop)))))

(define (delete-lower-notes)
  (d-MoveToBeginning)
  (let loop ((continue #f))
    (if (d-NextChord)
	(begin 
	  (if (d-CursorToNote (d-GetNoteFromTop 2))
	      (begin 
		(d-RemoveNoteFromChord)
		(set! continue #t)
		(loop continue))
	      (loop continue)))
	(if continue
	    (begin
	      (d-MoveToBeginning)
	      (loop  #f))))))

(define (pause) (disp "Pausing..." "Press any key" " "))


(d-MoveToBeginning)
(d-GoToEnd)
(d-Copy)
(d-AddAfter)
(d-Paste)
(d-MoveToBeginning)
(d-VoicePreset1)
(d-MoveCursorLeft)
(delete-highest-notes);;delete part 3
(delete-highest-notes);;delete part 2

(d-AddAfter)
(d-Paste)
(pause)
(d-MoveToBeginning)
(d-VoicePreset2)
(d-MoveCursorLeft)
(delete-highest-notes);;delete part 3
(delete-lower-notes);;delete part 1 (and anything extra)

(d-AddAfter)
(d-Paste)
(pause)
(d-MoveToBeginning)
(d-VoicePreset3)
(d-MoveCursorLeft)
(delete-lower-notes);;delete parts 2 and 1 (and anything extra)
(d-SimplifyTies)
(d-MoveToStaffUp)
(d-SimplifyTies)
(d-MoveToStaffUp)
(d-SimplifyTies)
)
