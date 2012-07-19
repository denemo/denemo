;;;FixSlursInStaff
(d-PushPosition)
(d-MoveToBeginning)
(let ()
  (define start #f)
  (let loop ()
    (if (d-IsSlurStart)
      (if start
	(d-ToggleBeginSlur)
	(set! start (GetPosition))))
    (if (d-IsSlurEnd)
      (if start
	(begin
	  (set! start #f)
	  (if (d-IsSlurStart)
	    (begin
	      (d-ToggleBeginSlur)
	      (d-ToggleEndSlur))))
	(d-ToggleEndSlur)))
	
    (if (d-NextChord)
      (loop)))
  (if start
    (begin
      (apply d-GoToPosition start)
      (d-ToggleBeginSlur))))
(d-PopPosition)