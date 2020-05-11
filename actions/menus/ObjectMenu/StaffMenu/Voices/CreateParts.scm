;;;CreateParts
(let ((num (string->number (d-GetUserInput (_ "Create Parts") "Give number of notes in chords: " "3"))))

(define (delete-lowest-note)
  ;delete lowest note of each chord
  (d-MoveToBeginning)
  (let loop () 
    (if (d-NextChord) 
	(begin 
	  (if (d-CursorToNote (d-GetNote)) (d-StagedDelete))
	  (loop)))))

(define (delete-upper-notes)
  (d-MoveToBeginning)
  (let loop ((continue #f))
    (if (d-NextChord)
	(begin 
	  (if (d-CursorToNote (d-GetNote 2))
	      (begin 
		(d-RemoveNoteFromChord)
		(set! continue #t)
		(loop continue))
	      (loop continue)))
	(if continue
	    (begin
	      (d-MoveToBeginning)
	      (loop  #f))))))



(d-MoveToBeginning)
(d-GoToEnd)
(d-Copy)

(let outer-loop ((voicenum num)) 
  (d-AddAfter)
  (d-Paste)
  (d-MoveToBeginning)
  (d-MuteStaff "unmute")
  (d-MoveCursorLeft)
  (if (positive? (1- voicenum))
    (let inner-loop ((n (1- voicenum)))
      (delete-lowest-note) 
      (if (positive? (1- n))
	(inner-loop (1- n)))))
  (if (not (= voicenum num))
    (delete-upper-notes))
  (if (positive? (1- voicenum))
    (outer-loop (1- voicenum))))
(d-SimplifyTies)
(d-MoveToStaffUp))
