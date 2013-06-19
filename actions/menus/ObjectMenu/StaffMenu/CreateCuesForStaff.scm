;;;CreateCues
(let ()
  (define (whole-measure-rest)
    (d-Directive-chord? "WholeMeasureRest"))

    ;;; find-block looks at the measures from the cursor onwards returning with the cursor on the last of a block of wmrs (#t) or at the end if none (#f)
  (define (find-block)
    (define count 0)
    (disp "find-block called")
    (let loop ()
      (if (whole-measure-rest)
	(begin
	  (set! count (+ 1 count))
	  (if (d-MoveToMeasureRight)
	    (loop)))
	  (begin ;; not on a whole measure rest
	    (if (zero? count)
	      (if (d-MoveToMeasureRight)
		(loop)))
	    (if (< count 3) ;;; do not cue for two or less rests
	      (begin
		(set! count 0)
		(if (d-MoveToMeasureRight)
		  (loop))))))
      (if (< count 3)
	#f
	(begin ;;;we have found a block of whole measure rests and are one beyond or at the end
	  (if (not (whole-measure-rest))
	    (d-MoveToMeasureLeft))
	  #t))))

    (define (copy-measure);;;FIXME get prevailing clef and insert it before the copy and remove it after
      (while (d-PrevObjectInMeasure))
      (d-SetMark)
      (while (d-NextObjectInMeasure))
      (d-SetPoint)
      (d-Copy))

;;; get-cue goes through the staffs from the top down looking for one (not the starting one) with music
;;; if it finds it it copies the measure to the clipboard and returns the staff-name	  
    (define (get-cue staff-name)
      (define cue-name #f)
      (disp "get-cue called for " staff-name "\n")
      (d-PushPosition)
      (while (d-MoveToStaffUp))
      (let loop ()
	(if (equal? staff-name (d-StaffProperties "query=denemo_name"))
	  (if (d-MoveToStaffDown)
	    (loop))	  
	  (if (not (whole-measure-rest))
	    (begin ;; we have a measure that we can use as a cue so copy it
	      (copy-measure)
	      (set! cue-name (d-StaffProperties "query=denemo_name")))
	    (if (d-MoveToStaffDown)
	      (loop)))))
    (d-PopPosition)      
    cue-name)

;;; actual code
  (d-EvenOutStaffLengths)
  (d-PushPosition)
  (d-GoToBeginning)
  (while (find-block)
    (let ((staff-name (d-StaffProperties "query=denemo_name")))
      (define cue (get-cue staff-name))
      (disp "found block with cue " cue "\n")
      (if cue
	(begin
	  (d-DeleteObject)
	  (StandAloneDirectiveProto (cons "CueName" (string-append " s1*0^\\markup \\italic { "cue "}"))  #t "\ne"       )
	  (d-SmallFontStart)
	  (d-Paste)
	  (d-SmallFontEnd)) ;;;FIXME Insert the previously prevailing clef, or delete the one at the start of the cue if it is the same	  
	(disp "No cue possible\n")))
	(d-MoveToMeasureRight))
  (d-PopPosition))

