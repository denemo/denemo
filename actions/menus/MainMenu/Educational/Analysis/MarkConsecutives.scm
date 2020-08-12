;;;MarkConsecutives
(let ((tag "MarkConsecutives")(consecutives '()))
  (define (mark-position pos)
      (define musobj (caar (cdr pos)))
      (d-GoToPosition  #f #f (musobj.measure musobj) (musobj.horizontal musobj))
      (d-DirectivePut-chord-graphic tag "CrossSign"))

(define (delete-lowest-notes)
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
(if (d-GetSaved)
	(begin
		(d-MoveToStaffUp)
		(d-MoveToBeginning)
		(d-DirectiveDelete-chord tag)
		(while (d-NextChord) (d-DirectiveDelete-chord tag))
		(d-MoveToBeginning)
		(d-GoToEnd)
		(d-Copy)
		(d-MoveToBeginning)
		(d-VoicePreset1)
		(d-MoveCursorLeft)
		(delete-lowest-notes);;delete part 3
		(delete-lowest-notes);;delete part 2

		(d-AddAfter)
		(d-Paste)

		(d-MoveToBeginning)
		(d-VoicePreset2)
		(d-MoveCursorLeft)
		(delete-lowest-notes);;delete part 3
		(delete-upper-notes);;delete part 1 (and anything extra)

		(d-AddAfter)
		(d-Paste)

		(d-MoveToBeginning)
		(d-VoicePreset3)
		(d-MoveCursorLeft)
		(delete-upper-notes);;delete parts 2 and 1 (and anything extra)

		(d-MoveToStaffUp)
		(d-MoveToStaffUp)
		(d-MoveToBeginning)

			
		(set! consecutives  (MapToAbstractionMovement (CreateAbstractionMovement) AM::consecutive5th AM::consecutive8th))
		;(disp "consecs " consecutives " \n")
		(d-InfoDialog (string-append (number->string (length consecutives)) " consecutives were found"))
		(d-SetSaved #t)
		(d-ReloadScore)
		(d-MoveToMovementBeginning)
		(for-each mark-position consecutives))
	(d-WarningDialog (_ "Score must be saved first"))))