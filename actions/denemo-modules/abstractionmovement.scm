(define-module (actions denemo-modules abstractionmovement3)
	#:export (
		CreateAbstractionMovement
		PasteAbstractionMovement
		SearchForSimultaneousInterval
		SearchForSimultaneousIntervalFromBase
		SearchForConsecutiveIntervalProgression	
		))

; This is the prototype of a system that creates an abstraction of the movement. 
; In the end there is a list of list of musobj-objects which are vertically aligned. Different durations are equalized out by inserting repeated notes.
; Only notes, chords and rests are saved currently. Duration is ignored. The start-tick value is used to check if notes need repetition.
; Uses ANS (Abstract Note System, defined in actions/ans.scm) 
; Uses musobj (A structure for musical objects, defined in actions/denemo.scm)
; Currently all functions are wrapped in a single big program. This way they do not pollute the global namespace, but they are called each time an Abstraction-Movement gets created, which is only needed after a score-change.

; Functions in this file. 
;;(CreateListStaffTicksInMeasures)
;;(ListStaffTicksInMeasures->StaffOffsetTickList listy)
;;(insert-into-list listy position what )
;;(duplicate-item-in-list listy position)
;;(list-equalizer! what . lists)
;;(insert-deep movement staffnumber position what) 
;;(duplicate-deep movement staffnumber position)
;;(GetStartTickMinimum listy)
;;(Abstraction::CreateMusObj)
;;(createFinalList)
;;(fill-with-redundancy! movement)
;;(CreateAbstractionMovement)

(define (CreateAbstractionMovement)

;Nearly any of the following functions are just crude prototypes. They are crude because they work with lists instead of a good data type like an red/black-tree. We need a data type here that is a list of list or allows simultanious items, allows refering to a single item and allows inserting. The result here is that all is done with list, appending, splitting, copying complete lists around instead of micro-changes.

;Create a list which holds a tick-sum for each measure. It counts the actual length, even if the measure is overful or underful. It assumes 100% fill for an empty measure.
(define (CreateListStaffTicksInMeasures)
(define return #f)
(d-PushPosition)
(d-MoveToBeginning)
(set! return
	(let loop ((listy (list #f)) )
		(if (EmptyMeasure?)
			(append! listy (list (duration::GetWholeMeasureInTicks)))
			(append! listy (list (GetMeasureTicks))))
		(if (d-MoveToMeasureRight)
			(loop listy)
			(list-tail listy 1))))
(d-PopPosition)
return)	


;Wants a list created by (CreateListStaffTicksInMeasures). 
;returns the tick offset for each measure. Each list position is equal to a measure number. There is no measure 0, so its #f
;The offset is each measure, added to all measure before it.
(define (ListStaffTicksInMeasures->StaffOffsetTickList listy)
	(define lstlength (length listy))
	(let loop ((counter 0)(return (list #f)))
			(if (= counter (1+ lstlength))
				return
				(loop (1+ counter) (append! return (list (apply + 0 (list-head listy counter)))))))) ; list-head excludes the given position. This means if you give a measure number this measure will be included. Same counting as (lenght list)

;Create a list of OffsetTickLists. Each list is a staff, which is a staff of measures.
;All countings from 1. Positions 0 are #f for both the outer and all inner lists.
(define (CreateOffsetTickListMovement)
	(define return (list #f))
	(d-PushPosition)
	(d-MoveToMovementBeginning)
	(let loop ()
		(append! return (list (ListStaffTicksInMeasures->StaffOffsetTickList (CreateListStaffTicksInMeasures))))
		(if (and (d-MoveToStaffDown) (d-MoveToBeginning))
			(loop)))
	(d-PopPosition)
	return)
				

;Very slow version of insert-into-list
(define (insert-into-list listy position what )
	(append
		(list-head listy position)
		(copy-tree (list what))
		(list-tail listy position)))
		
;Variant of insert-into-list that duplicates an item
(define (duplicate-item-in-list listy position)
	(append
		(list-head listy position)
		(copy-tree (list (list-ref listy position)))
		(list-tail listy position)
								))
								
;Make all lists equal in length by appending 'what' to their tails
;;Usage with a list of lists: (apply list-equalizer! #f listoflists)
(define (list-equalizer! what . lists)
	(define longest (reduce max 0 (map length lists)))
	(define (extend listy)
		(append! listy (copy-tree (make-list (- longest  (length listy))  what)))) ; compare given list with longest and fill with #f
	(map extend lists)
)						
								
;Nearly any of the following functions are just crude prototypes. They are crude because they work with lists instead of a good data type like an red/black-tree. We need a data type here that is a list of list or allows simultanious items, allows refering to a single item and allows inserting. The result here is that all is done with list, appending, splitting, copying complete lists around instead of micro-changes.
;A slow version to change one list in the list of lists. Replaces the whole inner list with a new one instead of modifying the original one. 
(define (insert-deep movement staffnumber position what)    
	(list-set!
	 movement ; outer list, the movement
	 staffnumber 
	 (insert-into-list (list-ref movement staffnumber) position what))) ; replaced with a new staff  
															
															
;Variant of insert-deep	to duplicate one item in an inner list. Like insert-deep its slow.											

(define (duplicate-deep movement staffnumber position)
(list-set!
	 movement ; outer list, the movement
	 staffnumber 
	 (duplicate-item-in-list (list-ref movement staffnumber) position))) ; create new list to replace the old one on the same position
															

;; Find the minimum value in a list of numbers. No check if only numbers!
(define (GetStartTickMinimum listy)
	(reduce min 0 (map musobj.start listy)))							
								
;; Create a MusObj but directly change it start-tick value to include the offset generated by (CreateOffsetTickListMovement)
(define (Abstraction::CreateMusObj)
	(define (getCurrentOffset musobj)
		(list-ref (list-ref OffsetTickList (musobj.staff musobj)) (musobj.measure musobj))) ; inner list refs gets the staff as list from OffsetTickList, outer list-ref the measure
	
	(define musobj (CreateMusObj))
	(set!musobj.start musobj (+ (getCurrentOffset musobj) (musobj.start musobj)))
	musobj)


;; creates a list of list of musob-j(ects). Each first level list is one staff.
;; All append! need an additional (list) because append merges only with lists.
(define (createFinalList)
   (define (staffLoop)  ;; Subproc to gather the information for one staff and return a list.
    (define stafflist (list #f))
     (if (or (Music?) (MeasureEmpty?)) ; if the first object is already a music item start right now. This prevents also a crash if the staff starts with a directive or else.
	  (append! stafflist (list (Abstraction::CreateMusObj))))
	  (let loop ()
	  	(if (d-MoveCursorRight)
			(begin
			(if (or (Music?) (MeasureEmpty?))
				(append! stafflist (list (Abstraction::CreateMusObj))))
				(loop))			 
			 (list-tail stafflist 1)))) ; Return list minus initial #f	  	
	 

;;; Body
  (d-MoveToMovementBeginning)
  (let loop ((final_list (list #f )) )
	(append! final_list (list (staffLoop))) ; First Staff is guaranteed to work, after first staff test for more staffs:
	(if (and (d-MoveToStaffDown) (d-MoveToBeginning)) 
		(loop final_list)
		(list-tail final_list 1)) ; If there is no staff, return the final list minus the initial #f		
    ); Final Loop end
  ); createFinalList end

(define (fill-with-redundancy! movement)
; prepare variables-
(define (ansrest? musobject) 
	(and
	(equal? (list +inf.0) (musobj.pitch musobject))
	(not (inf? (musobj.start musobject))) 		
	))	
							
(define positioncounter 0)
(define staffcounter 0)
(define MusObjectsOfCurrentPosition #f) ; All musobj of list-ref positioncounter 
(define minimum #f) ;The current lowest start-tick 
; Two sub-procs
(define (insertRestBeforeFalse musobject)
 (if (not musobject) ; if the object is a #f fill in a infinity rest before/to the current position
  	(insert-deep movement staffcounter positioncounter (make-musobj 'pitch (list +inf.0) 'movement #f 'staff #f 'measure #f 'horizontal #f 'metricalp #f 'start +inf.0 'duration #f)))	
	(set!musobj.pitch (list-ref (list-ref movement staffcounter) positioncounter) (musobj.pitch (list-ref (list-ref movement staffcounter) (- positioncounter 1))))
  	(set! staffcounter (+ staffcounter 1))) ; the next for-each iteration needs another staff
  	 
(define (checkAndChange listy) ; the main program to check and alter MusObjectsOfCurrentPosition
	(if (ansrest? (list-ref listy positioncounter))  ; rests need to gain a real pitch 
		(if (= 0 positioncounter)
			(set!musobj.pitch (list-ref listy positioncounter) (musobj.pitch (list-ref listy (+ positioncounter 1))))  ;or next object, for a leading rest in a staff
			(set!musobj.pitch (list-ref listy positioncounter) (musobj.pitch (list-ref listy (- positioncounter 1)))))) ;should take the pitch of the item before it 

	(if (= (musobj.start (list-ref listy positioncounter)) minimum) ; starts on minimum tick? 
		(set! staffcounter (+ staffcounter 1))  ;Current musobj starts at minimum tick, good.
		(begin notMinimum
			(if  (not (inf? (musobj.start (list-ref listy positioncounter)))) ; if infinity-rest just change the tick
				(begin 
					(duplicate-deep movement staffcounter (- positioncounter 1)) ; else copy a new musobj
					(set!musobj.start (list-ref listy (- positioncounter 1)) minimum)) ; change the created item to min start-tick
				(begin
					(set!musobj.pitch (list-ref listy positioncounter) (musobj.pitch (list-ref listy (- positioncounter 1)))) ; copy pitch
					(set!musobj.movement (list-ref listy positioncounter) (musobj.movement (list-ref listy (- positioncounter 1))))
					(set!musobj.staff (list-ref listy positioncounter) (musobj.staff (list-ref listy (- positioncounter 1))))
					(set!musobj.measure (list-ref listy positioncounter) (musobj.measure (list-ref listy (- positioncounter 1))))
					(set!musobj.horizontal (list-ref listy positioncounter) (musobj.horizontal (list-ref listy (- positioncounter 1))))
					(set!musobj.metricalp (list-ref listy positioncounter) (musobj.metricalp (list-ref listy (- positioncounter 1))))
					(set!musobj.start (list-ref listy  positioncounter) minimum))
			); if end
			
			(set! staffcounter (+ staffcounter 1))
										)); begin notMinimum & if minimum
); checkAndChange subProc.

(let loop ()
  (set! staffcounter 0); reset staffcounter before for-each, which loops through staffs
  (set! MusObjectsOfCurrentPosition (map (lambda (lst) (list-ref lst positioncounter)) movement))
    (if (member #f MusObjectsOfCurrentPosition)
	(if (every not MusObjectsOfCurrentPosition) ; If all are #f the movement is at its end
		"Abstraction Movement ready" ; The End. "movement" is ready.	
		(begin 
			(for-each insertRestBeforeFalse MusObjectsOfCurrentPosition) (loop))) ;fill the tail with an infinity rest for each #f on current position.	
	(begin mainBlock ; no #f on current position
		(set! minimum (GetStartTickMinimum MusObjectsOfCurrentPosition)) 
 		 (for-each checkAndChange movement) ; always feed with the current state of the whole movement
		(set! positioncounter (1+ positioncounter))
		(loop)
									)); if member #f? & begin mainBlock
)); fill-with-redundancy! end
	

; Create the abstraction movement in multiple steps. They are all desctructive.
;;0 Create a helper list to with the tick offset for each measure.
;;1 parse everything, save the music as musobj. Notes, Chords, Rests
;;2 make all length equal by adding rests in the infinity to the ends.
;;3 one final #f to all staffs. The end is reached when all position return #f instead of a musobj
;;4 check if all start-ticks are the same, if not duplicate notes to fill the gaps
	(define OffsetTickList #f)
	(define movement #f)
	(set! OffsetTickList (CreateOffsetTickListMovement))
	(set! movement (createFinalList))	
	(apply list-equalizer! (make-musobj 'pitch (list +inf.0) 'movement #f 'staff #f 'measure #f 'horizontal #f 'metricalp #f 'start +inf.0  'duration #f) movement)
	(map (lambda (lst) (append! lst (list #f))) movement)
	(fill-with-redundancy! movement)
	movement)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Functions that use the abstractionmovement;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;PasteAbstractionMovement creates a new window and visualises the contents of an abstractionmovement there.
(define (PasteAbstractionMovement abstractionmovement)
 ; For each staff (primary list in abstractionmovement)
 ; do inserting the pitch for every (for-each) object 
	(d-NewWindow) ; creates a new movement with the same number of staffs
	(d-MoveToMovementBeginning)
	(for-each (lambda (staff) 
		(d-AddAfter)
		(d-MoveToBeginning)
		(for-each (lambda (object)
			(if (not object)
				(d-MoveToStaffDown)  ; move one down for the next iteration
				(ANS::InsertNotes (musobj.pitch object) 0 384)))
			 staff))
	abstractionmovement))	


(define (SearchForSimultaneousIntervalProto intervalPairFunction abstractmovement forbiddenintervals)
  (define now #f) ;prepare
  (define returnlist (list #f))
  (define (getVerticalPosition positioncounter)
	(map (lambda (lst) (list-ref lst positioncounter)) abstractmovement))
  (define (extractPitchesFromVerticalPosition musobj-list)
	(map (lambda (object) (list-ref (musobj.pitch object) 0)) musobj-list))
  (define (extractPosition musobj-list)
	(define object (list-ref musobj-list 0))
	;just take staff 0, they have the same position anyway. Return the whole position.
	(list (musobj.movement object)  (musobj.staff object)  (musobj.measure object)  (musobj.horizontal object)))
  (define maxcounter (- (length (list-ref abstractmovement 0)) 2)) ; just take staff 0, they all have the same length. -2 because the counter goes from 0 (-1) and the last item is #f.
;Body
  (set! forbiddenintervals (map ANS::IntervalGetSteps forbiddenintervals))
  (let search ((positioncounter 0))
    (set! now  (ANS::CreateIntervalsFromPairs (intervalPairFunction (extractPitchesFromVerticalPosition (getVerticalPosition positioncounter)))))	
	(if (ANS::IntervalMember? now forbiddenintervals) ; the core function
		(append! returnlist (list (cons 'simultanious (list (extractPosition (getVerticalPosition positioncounter)))))))
    (if (= positioncounter maxcounter) 
   	 (list-tail returnlist 1)  ; The End.  get rid of the initial #f for the final return value
   	 (search (1+ positioncounter)))))  ; Run again with next position.


;Search for Simultanious Intervals wants an abstract movement followed by any number of interval-symbols like 'p5 or 'm3
;;Returns a list of positions where forbidden intervals live.
;;TODO: One mode to look only on metric important positions. For this the ans intervalpair (interval (lower . higher)) must be upgraded to use the complete musobj. in lower and higher. this will also solve the problems of correct location spotting.
(define (SearchForSimultaneousInterval abstractmovement . forbiddenintervals)
  (SearchForSimultaneousIntervalProto GetUniquePairs abstractmovement forbiddenintervals))
 

;SearchForSimultaneousIntervalFromBase wants an abstract movement followed by any number of interval-symbols like 'p5 or 'm3
;;Returns a list of positions where forbidden intervals live but only looks for pairs with the base/lowest note involved.
;;TODO: One mode to look only on metric important positions. For this the ans intervalpair (interval (lower . higher)) must be upgraded to use the complete musobj. in lower and higher. this will also solve the problems of correct location spotting.
(define (SearchForSimultaneousIntervalFromBase abstractmovement . forbiddenintervals)
  (SearchForSimultaneousIntervalProto GetUniquePairsFilterLowest abstractmovement forbiddenintervals))

;Seach for successive interval in one voice alone!
;SearchForSuccessiveIntervalProgression 


;SearchForConsecutiveIntervalProgression wants an abstract movement followed by any number of interval-symbols like 'p5 or 'm3
;Returns a list of positions where forbidden interval progressions live.
;Example: (SearchForConsecutiveIntervalProgression (CreateAbstractionMovement) 'p5 'p1)
(define (SearchForConsecutiveIntervalProgression abstractmovement . forbiddenintervals)
  (define now #f) ;prepare
  (define next #f) ;prepare
  (define returnlist (list #f))
  (define (getVerticalPosition positioncounter)
	(map (lambda (lst) (list-ref lst positioncounter)) abstractmovement))
  (define (extractPitchesFromVerticalPosition musobj-list)
	(map (lambda (object) (list-ref (musobj.pitch object) 0)) musobj-list))
  (define (extractPosition musobj-list)
	(define object (list-ref musobj-list 0))
	;just take staff 0, they have the same position anyway. Return the whole position.
	(list (musobj.movement object)  (musobj.staff object)  (musobj.measure object)  (musobj.horizontal object)))
  (define maxcounter (- (length (list-ref abstractmovement 0)) 3)) ; just take staff 0, they all have the same length. -3 because the counter goes from 0 (-1) and the last item is #f so we want to stop when on the last real pair (-2)
;Body
;(for-each (lambda (staff)  (disp (length staff) " " staff)(newline)) abstractmovement) ; display all  
  (set! forbiddenintervals (map ANS::IntervalGetSteps forbiddenintervals))
  (let search ((positioncounter 0))
    (set! now  (ANS::CreateIntervalsFromPairs (GetUniquePairs (extractPitchesFromVerticalPosition (getVerticalPosition positioncounter)))))	
    (set! next  (ANS::CreateIntervalsFromPairs (GetUniquePairs (extractPitchesFromVerticalPosition (getVerticalPosition (1+ positioncounter))))))   
     
     ;Check for various Consecutive Interval Progressions and return each position with a tag of which type as (cons tag (list position))
     (cond 
	((ANS::ConsecutiveOpen? now next forbiddenintervals)
		(append! returnlist (list (cons 'consecutive_open (list (extractPosition (getVerticalPosition positioncounter)))))))
	
	((ANS::ConsecutiveAnti? now next forbiddenintervals)
		(append! returnlist (list (cons 'consecutive_anti (list (extractPosition (getVerticalPosition positioncounter)))))))

	((ANS::ConsecutiveCrossed? now next forbiddenintervals)		
		(append! returnlist (list (cons 'consecutive_crossed (list (extractPosition (getVerticalPosition positioncounter)))))))
     
    ((ANS::ConsecutiveHidden? now next forbiddenintervals)		
		(append! returnlist (list (cons 'consecutive_hidden (list (extractPosition (getVerticalPosition positioncounter)))))))
		
	((ANS::ConsecutiveHiddenCrossed? now next forbiddenintervals)		
		(append! returnlist (list (cons 'consecutive_hidden_crossed (list (extractPosition (getVerticalPosition positioncounter)))))))	
     );cond    

    (if (= positioncounter maxcounter) 
   	 (list-tail returnlist 1)  ; The End.  get rid of the initial #f for the final return value
   	 (search (1+ positioncounter)))     ))  ; Run again with next position.




