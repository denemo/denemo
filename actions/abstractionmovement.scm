; This is the prototype of a system that creates an abstraction of the movement. 
; In the end there is a list of list of musobj-objects which are vertically aligned. Different durations are equalized out by inserting repeated notes.
; Only notes, chords and rests are saved currently. Duration is ignored. The start-tick value is used to check if notes need repetition.
; Uses ANS (Abstract Note System, defined in actions/ans.scm) 
; Uses musobj (A structure for musical objects, defined in actions/denemo.scm)
; Currently all functions are wrapped in a single big program. This way they do not pollute the global namespace, but they are called each time an Abstraction-Movement gets created, which is only needed after a score-change.

; Functions in this file. 
;;(insert-into-list listy position what )
;;(duplicate-item-in-list listy position)
;;(list-equalizer! what . lists)
;;(insert-deep movement staffnumber position what) 
;;(duplicate-deep movement staffnumber position)
;;(GetStartTickMinimum listy)
;;(createFinalList)
;;(fill-with-redundancy! movement)
;;(createAbstractionMovement)

(define (createAbstractionMovement)

;Nearly any of the following functions are just crude prototypes. They are crude because they work with lists instead of a good data type like an red/black-tree. We need a data type here that is a list of list or allows simultanious items, allows refering to a single item and allows inserting. The result here is that all is done with list, appending, splitting, copying complete lists around instead of micro-changes.
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
								
;; creates a list of list of musobj(ects). Each first level list is one staff.
;; All append! need an additional (list) because append merges only with lists.

(define (createFinalList)
    (define (staffLoop)  ;; Subproc to gather the information for one staff and return a list.
    (define stafflist (list #f))
    (if (music?) ; if the first object is already a music item start right now. This prevents also a crash if the staff starts with a directive or else.
	  (append! stafflist (list (createMusObj))))
	  (let loop ()
	  	(if (d-NextChord)
			(begin
				(append! stafflist (list (createMusObj)))
				(loop))
	  	 (list-tail stafflist 1) ; Return list minus initial #f
	  	)
	  ); subloop end
     );staffLoop end

;;; Body
  (d-MoveToMovementBeginning)
  (let loop ((final_list (list #f )) )
	(append! final_list (list (staffLoop))) ; First Staff is guaranteed to work, after first staff test for more staffs:
	(if (and (d-MoveToStaffDown) (not (d-MoveToBeginning))) ; TODO!!! MoveToBeginning is wrong and returns #f
		(loop final_list)
		(list-tail final_list 1)) ; If there is no staff, return the final list minus the initial #f		
    ); Final Loop end
  ); createFinalList end

(define (fill-with-redundancy! movement)
; prepare variables-
(define (ansrest? musobject) 
	(and
	(equal? (list "900") (musobj.pitch musobject))
	(not (inf? (musobj.start musobject))) 		
	))	
							
(define positioncounter 0)
(define staffcounter 0)
(define MusObjectsOfCurrentPosition #f) ; All musobj of list-ref positioncounter 
(define minimum #f) ;The current lowest start-tick 
; Two sub-procs
(define (insertRestBeforeFalse musobject)
 (if (not musobject) ; if the object is a #f fill in a infinity rest before/to the current position
  	(insert-deep movement staffcounter positioncounter (make-musobj 'pitch (list "900") 'measure #f 'start +inf.0 'end #f 'duration #f)))	
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
					(set!musobj.pitch (list-ref listy positioncounter) (musobj.pitch (list-ref listy (- positioncounter 1))))
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
;;1 parse everything, save the music as musobj. Notes, Chords, Rests
;;2 make all length equal by adding rests in the infinity to the ends.
;;3 one final #f to all staffs. The end is reached when all position return #f instead of a musobj
;;4 check if all start-ticks are the same, if not duplicate notes to fill the gaps
	(define movement #f)
	(set! movement (createFinalList))
	(apply list-equalizer! (make-musobj 'pitch (list "900") 'measure #f 'start +inf.0 'end #f 'duration #f) movement)
	(map (lambda (lst) (append! lst (list #f))) movement)
	(fill-with-redundancy! movement)
	movement
)
