(use-modules (srfi srfi-1)) ; List library

#!(define-module (actions denemo-modules abstractionmovement3)
    #:export (
        CreateAbstractionMovement
        PasteAbstractionMovement
        SearchForSimultaneousInterval
        SearchForSimultaneousIntervalFromBase
        SearchForConsecutiveIntervalProgression 
        )) !#

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

(define mainBlock #f)(define notMinimum #f) ;;; RTS - I don't know what these are, but for some reason they appear below in a context where they need to be defined.

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
        (append! listy (copy-tree (make-list (- longest  (length listy))  what)))) ; compare given list with longest and fill with 'what'
    (map extend lists)
)                       
                                
;Nearly any of the following functions are just crude prototypes. They are crude because they work with lists instead of a good data type like an red/black-tree. We need a data type here that is a list of list or allows simultanious items, allows refering to a single item and allows inserting. The result here is that all is done with list, appending, splitting, copying complete lists around instead of micro-changes.
;A slow version to change one list in the list of lists. Replaces the whole inner list with a new one instead of modifying the original one. 
(define (insert-deep movement staffnumber position what)    
    (list-set!
     movement ; outer list, the movement
     staffnumber 
     (insert-into-list (list-ref movement staffnumber) position what))) ; replaced with a new staff  
                                                            
                                                            
;Variant of insert-deep to duplicate one item in an inner list. Like insert-deep its slow.                                          

(define (duplicate-deep movement staffnumber position)
(list-set!
     movement ; outer list, the movement
     staffnumber 
     (duplicate-item-in-list (list-ref movement staffnumber) position))) ; create new list to replace the old one on the same position
                                                            

;; Find the minimum value in a list of numbers. No check if only numbers!
(define (GetStartTickMinimum listy)
    (reduce min 0 (map musobj.start listy)))                            
                                
;; Create a MusObj but directly change its start-tick value to include the offset generated by (CreateOffsetTickListMovement)
(define (Abstraction::CreateMusObj)
    (define (getCurrentOffset musobj)
        (list-ref (list-ref OffsetTickList (musobj.staff musobj)) (musobj.measure musobj))) ; inner list refs gets the staff as list from OffsetTickList, outer list-ref the measure
    
    (define musobj (CreateMusObj))
    (set!musobj.start musobj (+ (getCurrentOffset musobj) (musobj.start musobj)))
    musobj)


;; creates a list of list of musobj(ects). Each first level list is one staff.
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
    (insert-deep movement staffcounter positioncounter (make-musobj 'pitch (list +inf.0) 'movement #f 'staff #f 'measure #f 'horizontal #f 'metricalp #f 'start +inf.0 'duration #f 'baseduration #f 'dots #f)))    
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
                    (set!musobj.start (list-ref listy  positioncounter) minimum)
                    )
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
;;5 delete the tailing #f from the movement
    (define OffsetTickList #f)
    (define movement #f)
    (set! OffsetTickList (CreateOffsetTickListMovement))
    (set! movement (createFinalList))   
    (apply list-equalizer! (make-musobj 'pitch (list +inf.0) 'movement #f 'staff #f 'measure #f 'horizontal #f 'metricalp #f 'start +inf.0  'duration #f 'baseduration #f 'dots #f) movement)
    (map (lambda (lst) (append! lst (list #f))) movement)
    (fill-with-redundancy! movement)
    (set! movement (map (lambda (lst) (drop-right lst 1)) movement))
    movement)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Functions that use the abstractionmovement;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;PasteAbstractionMovement creates a new Denemo-tab and visualises the contents of an abstractionmovement there.
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

;ApplyTestsToAbstractionMovementPositions takes functions and applies each to a value and returns a list of returnvalues.
;;Used in MapToAbstractionMovement
(define (ApplyTestsToAbstractionMovementPositions previous current next . functions)
    (filter (lambda (x) (not (not x)))
        (concatenate (concatenate 
             (map (lambda (proc) (call-with-values (lambda () (proc previous current next)) list)) 
                   functions)))))

;MapToAbstractionMovement wants an abstractionsmovement and functions
;; Each function will be applied to a vertical position (all notes which sound at the same time). The return values will be gathered in a list and then it advcances to the next vertical position until the end of the abstractionmovement.
;; Compatible functions must accept three parameters: Three lists of MusObjs: All notes from the previous, current and next position. Even if the functions discard one or two of these position internaly.
;; Compatible functions must return a list which members are either #f or pairs (cons 'tag data-you-like). In the end any #f will be deleted automatically. "Data-you-like" is mainly there to let Denemo find the place where the error is so you better include musobjs there.
(define (MapToAbstractionMovement abstractmovement . functions)
    (define return #f)
    (set! abstractmovement (apply map list abstractmovement)) ; sync the voices vertically "in chords" by creating new list from each of the sublists. They all have the same length.
    (set! return (map  ; this needs srfi-1 map which handles unequal list lengths
                (lambda (previous current next) (apply ApplyTestsToAbstractionMovementPositions previous current next functions))
                 (cons #f abstractmovement)
                 abstractmovement
                 (append (cdr abstractmovement) (list (last abstractmovement)))))
    (filter (lambda (x) (not (null? x))) (concatenate return)))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Tests for MapToAbstractionMovement;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Basic functions
;; "AM::CapitalCamelCase"

;Find consecutive interval progressions in two pairs of notes
(define* (AM::TestConsecutiveIntervalProgression previous current next proc interval tag #:optional (staffs #f))
    (map 
        (lambda (pair-current pair-next)             
            (if (proc
            (car (musobj.pitch (cdr pair-current)))  ;low1
            (car (musobj.pitch (car pair-current))) ;high1
            (car (musobj.pitch (cdr pair-next))) ;low2
            (car (musobj.pitch (car pair-next))) ;high1
            interval)
                (cons tag (cons pair-current pair-next))
                #f))            
        (if staffs ;its possible to only check certain voices which may be in the optional staffs var.
            (GetUniquePairs (map (lambda (x) (list-ref current x)) staffs))
            (GetUniquePairs current)) 
        (if staffs
            (GetUniquePairs  (map (lambda (x) (list-ref next x)) staffs))
            (GetUniquePairs next))))                
        
(define (AM::TestSimultaneousIntervalFromBaseMetricalMain previos current next interval tag)
    (define pairlist (GetUniquePairsFilterLowest current MusObj::minPitch))
    (map 
        (lambda (pair)
            (if (and  (duration::MetricalMain? (musobj.metricalp (car pair))) (duration::MetricalMain? (musobj.metricalp (cdr pair))) (= interval (MusObj::GetInterval (car pair) (cdr pair)))) ; if interval and both notes are on a metrical main position
                (cons tag pair)
                #f))
        pairlist))
        
(define (AM::TestSimultaneousIntervalFromBaseMetricalFirst previos current next interval tag)
    (define pairlist (GetUniquePairsFilterLowest current MusObj::minPitch))
    (map 
        (lambda (pair)
            (if (and  (= 1 (musobj.metricalp (car pair))) (= 1 (musobj.metricalp (cdr pair))) (= interval (MusObj::GetInterval (car pair) (cdr pair)))) ; if interval and both notes are on the first metrical position
                (cons tag pair)
                #f))
        pairlist))      
        
;AM::GenerateStaffList converts numbers and special symbols to a list of numbers which represents which of the staffs should be used for an AM::test
;;needs to know how many staffs there are and as many parameters as needed
;;parameters are plain numbers or special symbols
;; 'last  the last staff
;; 'first the first staff, but better use 0.
;; '-3-5  from 3 to 5. Note the leading dash. 
(define (AM::GenerateStaffList staffcount . parameter)
    (set! parameter (delete-duplicates parameter))
    parameter)
        
;Real Tests that can be used as MapToAbstractionMovement functions
;;"AM::lowerCamelCase"

;Just display all "chords", made of musobj
;;Debug only. Has a wrong return format.
(define (AM::display previous current next)
    (list current))
    
;Display all "chords" as lilypond pitches
;;Debug only. Has a wrong return format.
(define (AM::displayLilypond previous current next)
    (map (lambda (x) 
        (ANS::Ans2Ly (car (musobj.pitch x))))
        current))
        
(define (AM::consecutive5th previous current next)
    (AM::TestConsecutiveIntervalProgression previous current next ANS::ConsecutiveOpen? 1 'consecutive5th))

(define (AM::consecutive8th  previous current next)
    (AM::TestConsecutiveIntervalProgression previous current next ANS::ConsecutiveOpen? 0 'consecutive8th))

(define (AM::crossed5th previous current next)
    (AM::TestConsecutiveIntervalProgression previous current next ANS::ConsecutiveCrossed? 1 'crossed5th))

(define (AM::crossed8th previous current next)
    (AM::TestConsecutiveIntervalProgression previous current next ANS::ConsecutiveCrossed? 0 'crossed8th))
    
(define (AM::hidden5th previous current next)
    (AM::TestConsecutiveIntervalProgression previous current next ANS::ConsecutiveHidden? 1 'hidden5th))

(define (AM::hidden8th previous current next)
    (AM::TestConsecutiveIntervalProgression previous current next ANS::ConsecutiveHidden? 0 'hidden8th))
    
(define (AM::anti5th previous current next)
    (AM::TestConsecutiveIntervalProgression previous current next ANS::ConsecutiveAnti? 1 'anti5th))

(define (AM::anti8th previous current next)
    (AM::TestConsecutiveIntervalProgression previous current next ANS::ConsecutiveAnti? 0 'anti8th))    

(define (AM::hiddencrossed5th previous current next)
    (AM::TestConsecutiveIntervalProgression previous current next ANS::ConsecutiveHiddenCrossed? 1 'hiddencrossed5th))

(define (AM::hiddencrossed8th previous current next)
    (AM::TestConsecutiveIntervalProgression previous current next ANS::ConsecutiveHiddenCrossed? 0 'hiddencrossed8th))


    
(define (AM::simultaneousFromBaseMetricalMain4th previos current next)
    (AM::TestSimultaneousIntervalFromBaseMetricalMain previos current next -1 'simultaneousBaseMain4th))

(define (AM::simultaneousFromBaseMetricalFirst8th previos current next)
    (AM::TestSimultaneousIntervalFromBaseMetricalFirst previos current next 0 'simultaneousBaseFirst8th))

(define (AM::simultaneousFromBaseMetricalFirst5th previos current next)
    (AM::TestSimultaneousIntervalFromBaseMetricalFirst previos current next 1 'simultaneousBaseFirst5th))
    
;This function is not a test itself but generates a test-function
(define (AM::generateHidden5th stafflist)
    (lambda (previous current next) (AM::TestConsecutiveIntervalProgression previous current next ANS::ConsecutiveHidden? 1 'hidden5th stafflist)))

;This function is not a test itself but generates a test-function
(define (AM::generateHidden8thstaffs stafflist)
    (lambda (previous current next) (AM::TestConsecutiveIntervalProgression previous current next ANS::ConsecutiveHidden? 0 'hidden8th stafflist)))
    
