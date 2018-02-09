#!
(define-module (actions denemo-modules selection)
    #:export (
        MoveToEndOfSelection
        NextSelectedObjectAllStaffs
        SingleAndSelectionSwitcher
        MapToSelection
        ApplyToTaggedSelection
        SingleAndTaggedSelectionSwitcher
        NextChordInSelection
        DenemoPaste
        SchemeCopy
        ProcessSchemeCopyBufferMusObj
        SchemePaste
    )
    #:use-module (ice-9 optargs)) !#



;;; move to selection end if cursor is in selection, return #f if not in selection
(define  (MoveToEndOfSelection)
  (if (d-IsInSelection)
        (let loop ()
            (if (d-MoveCursorRight)
                (if (d-IsInSelection)
                    (loop)
                    (d-MoveCursorLeft))))
        #f))
                
(define (MoveToSelectionBeginningInThisStaff)
    (define staffPosition (d-GetStaff))
    (define rememberPosition (GetPosition))
    (if (d-GoToSelectionStart)
        (begin 
            (d-GoToPosition #f staffPosition #f 1)
            (if (d-IsInSelection)
                #t
                (d-GoToSelectionStart))) ; single staff selection, not at start of bar 
        #f)) ; no selection at all. 

;Next object in selection for all staffs
(define (NextSelectedObjectAllStaffs)
    (define lastposition (GetPosition))
    (if (and (d-MarkStatus) (d-IsInSelection))
        (if (d-NextSelectedObject)
          #t ; found one. End
           (if (d-MoveToStaffDown) ; no selected item left in the current staff. check one down.
                (if (and (d-IsInSelection) (MoveToSelectionBeginningInThisStaff)) 
                    #t ; found a selection in the lower staff
                    (begin (apply d-GoToPosition lastposition ) #f)) ; reset cursor to the last known selection position and end.
                  #f)) ; no staff below
      #f)); no selection or cursor not in selection   

        
;SingleAndSelectionSwitcher by Nils Gey Jan/2010
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Automatically applies a script to a whole selection. You can give different commands or command blocks with (begin) for single items or whole selections. You can enter a complete scheme script with (),  arguments and everything you would want to run standalone. Don't forget to escape chars like  \" . You can even use a complete (begin ) block.
;But attention! SingleAndSelectionSwitcher will still try to apply the given script to each of the single items alone. If you need a script which differs completly in beaviour for single/selection you have to write your own. You have to take out the (let loop () section for this and write your own selection part there.
;The applied script itself has to take care if the command can be applied to each potential item. If you want only notes/chords/rests you have to make sure the script does not abort on other objects. Its the same as giving proper return values for a single item, just return #f if a command is not possible for an item. While a single item just returns an error if you don't do it correctly, but does no harm otherwise, a script applied to a selection will stop on that item and leaves you on half on the way.
;Return values are the return values the script itself gives.
;The third, optional, parameter can prevent an object from be processed. By default this parameter is #t so the command will be will be applied to any object in the selection and let the command itself decide what to do (or just do nothing). By giving the third optional argument you can specify additional conditions, for example with GetType. In general: Insert test conditions here, if #t the current object will be processed, otherwise it will be skipped.
;Example: (SingleAndSelectionSwitcher d-AddDot d-ToggleStaccato) ; this is nonsense, but valid. It will add a dot for single notes or toggle staccato for the whole selection.

(define* (SingleAndSelectionSwitcher commandsingle #:optional (commandselection commandsingle) (onlyFor True)) ; Amazingly commandsingle is already defined on spot so that it can be used again in the same line to define commandselection 

    (if (or (not (d-HasSelection)) (d-IsInSelection))
        (begin

            (if (string? commandsingle) ; support for old scripts. They passed the complete string of scheme as parameter
                (set! commandsingle (eval-string (string-append "(lambda () " commandsingle " )"))))
            (if (string? commandselection)
                (set! commandselection (eval-string (string-append "(lambda () " commandselection " )"))))      
            (if (and DenemoPref_applytoselection (d-MarkStatus))
                (ForEachToSelection commandselection onlyFor)
                (commandsingle)))
        (d-WarningDialog (_ "Cursor is not inside selection"))))

; MapToSelection is like schemes (map) mixed with ApplyToSelection. Use a proc on all selection items and gather all proc return values in a list. You can give an optional test, only items which return #t are processed.
(define* (MapToSelection proc #:optional (onlyFor True))
    (define return (list #f))   ; prepare return list
    (define (gather)
        (if (onlyFor) ; test the current item
            (append! return (list (proc))) ; execute the proc and append its return value as listmember to the returnlist
            #f))
    (if (and DenemoPref_applytoselection (d-MarkStatus)) ; only if preferences allow it and if there is a selection at all
        (begin 
            (d-PushPosition)
            (d-GoToSelectionStart)
            (gather) ; start one without selection testing. We already know we have a selection and RepeatProcWhileTest tests first which results in ignoring the first selected item.
            (RepeatProcWhileTest gather NextSelectedObjectAllStaffs) ; Use the proc/gather function on all items in the selection
            (d-PopPosition)
            (list-tail return 1))           
        #f))

;ForEachToSelection applies the command to each item in the selection. The return value is unspecified. Faster than MapToSelection.
(define* (ForEachToSelection proc #:optional (onlyFor True))
    (define (apply)
        (if (onlyFor) ; test the current item
            (proc)          
            #f))
    (if (and DenemoPref_applytoselection (d-MarkStatus)) ; only if preferences allow it and if there is a selection at all
        (begin 
            (d-PushPosition)
            (d-GoToSelectionStart)
            (apply) ; start one without selection testing. We already know we have a selection and RepeatProcWhileTest tests first which results in ignoring the first selected item.
            (RepeatProcWhileTest apply NextSelectedObjectAllStaffs) ; Use the proc/gather function on all items in the selection
            (d-PopPosition)
            (if #f #f) ; return unspecified.
            )           
        #f))

;Three functions to tag any Denemo-object. Invisible to the user or lilypond.
(define (Tag) (d-DirectivePut-object-minpixels "select" 0))
(define (Untag) (d-DirectiveDelete-object "select"))
(define (Tag?) (d-DirectiveGetForTag-object "select" ))

;Search objects which were tagged by (Tag)
(define (NextTaggedObjectAllStaffs)
    (define position (GetPosition))
    (if (FindNextObjectAllStaffs Tag?)  
        #t
        (begin (apply d-GoToPosition position) #f)))
                    
;An alternative implementation of ApplyToSelection which works with (Tag) instead of the normal selection. This allows destructive changes which would normally destroy the Denemo-selection
;;Instead of a range, like the built-in selection, every item is tagged on its own. This is slower but allows items to be changed or deleted, which is not allowed otherwise
(define (ApplyToTaggedSelection proc)
    (if (ForEachToSelection Tag) ; ForEachToSelection tests: only for selections and if preferences allow it
        (let ()
            (d-GoToSelectionStart)
            (d-UnsetMark)
            (Untag) (proc)
            (RepeatProcWhileTest
                (lambda () (Untag) (proc)) ; The action happens here. Untag makes sure that we never encounter an endless loop because the of functions that move the cursor on their own and return to the tagged item so the movement instruction see below cannot advance.
                (lambda () ; movement/test for RepeatProc which returns #t or #f
                (if (Tag?) ; if the current object is already tagged stay. This is guaranteed to only happen once because next time it will be untagged by the line above.
                    #t
                    (NextTaggedObjectAllStaffs)))))
        #f)) ; no selection or not allowed by preferences

;A SingleAndSelectionSwitcherVariant that works with TaggedSelection which is more robust and works for more commands, but is slower.
;; For documentation see (SingleAndSelectionSwitcher) and (ApplyToTaggedSelection)
;; Works only with real functions, no deprecated support for string-commands like the original SingleAndSelectionSwitcher
(define* (SingleAndTaggedSelectionSwitcher commandsingle #:optional (commandselection commandsingle) (onlyFor True))
    (if (and DenemoPref_applytoselection (d-MarkStatus)) ; decide if single or selection.
        (ApplyToTaggedSelection (lambda () (if (onlyFor) (commandselection))))
        (commandsingle)))
        
(define NextChordInSelection (lambda () (if (d-NextSelectedObject) 
                        (if (Music?)
                                 #t
                                 (NextChordInSelection))
                        #f)))
(define FirstChordInSelection (lambda () (if (d-GoToMark)
                          (if (Music?)
                                 #t)
                          #f)))

; Paste by Nils Gey, 2011
;; Multistaff-Pasting always adds the complete part AFTER the current measure or fills any complete set of empty measures
;; Singlestaff-Pasting happens at the cursor position and will just paste whats in the clipboard
(define* (DenemoPaste #:optional (autocreatebarlines #f))
  (define (Paste)
    (define paste:multistaff? (d-GetClipObjType 1 0))
    (define paste:howmanystaffs
        (let loop ((n 0))
        (if (d-GetClipObjType (1+ n) 0)
        (loop (1+ n))
        (1+ n))))
    (define position:startmeasure (d-GetMeasure))
    (define position:startstaff (d-GetStaff))
    (define staff 0)
    (define count -1)       
    (define staffcountlist (list 0)) ; used for multistaff          
    (define (1+count!)
        (set! count (1+ count)))
    (define (1+staff!)
        (set! staff (1+ staff)))
    (define (MeasuresToPasteToEmpty?)
        (define position:measure (d-GetMeasure))    
        (not (any not (map (lambda (x) (ProbePosition None? #f (+ position:startstaff x) (1+ position:measure) 1)) staffcountlist))))
    (define (SplitMeasure!)
        (if paste:multistaff?
            (let ()
                (define position:measure (d-GetMeasure))            
                (define position:current (GetPosition))
                (for-each (lambda (x) 
                    (if (d-GoToPosition #f (+ position:startstaff x) (1+ position:measure) 1)
                        (d-SplitMeasure) 
                        (begin 
                            (d-GoToPosition #f (+ position:startstaff x) 1 1) 
                            (d-MoveToEnd) (d-SplitMeasure)))) ; for staff ends
                     staffcountlist)
                (apply d-GoToPosition position:current)
                (d-MoveToMeasureRight)) ; all needed empty measures got created         
            (d-SplitMeasure))) ; singlestaff is simple split.   
    (define (Put!) 
        (if (and autocreatebarlines (not paste:multistaff?) (not (UnderfullMeasure?)) (Appending?)) ; in a single-staff with AutoCreateBarlines #t Put! will create Barlines if the current measure is full, not MeasureBreak!
            (if (d-MoveToMeasureRight)
                        #t
                        (SplitMeasure!)))
        (d-PutClipObj staff count)) ; nothing special here. Just paste.             
    (define (MeasureBreak!)
        (if (and autocreatebarlines (not paste:multistaff?))
            #t ; no barline should be created by paste. Let Denemo decide if a measure is full or not.
            (if  (or (> staff 0) (MeasuresToPasteToEmpty?)) ; only the first staff needs to check if the next measure is empty or not. In Multistaff the first paste-round created all necessary empty measures for all other staffs so its just straight-forward pasting of objects.
                (d-MoveToMeasureRight)
                (SplitMeasure!))))
    (define (paste! staff count)        
        (case (d-GetClipObjType staff count)
            ;In order of occurence, to boost performance.
            ((0) (Put!))    ;note, rest, gracenote, chord
            ((8) (MeasureBreak!) ) ; Measurebreak
            ((15) (d-PutClipObj staff count))   ;lilypond-directive
            ((1) (d-PutClipObj staff count))    ;tuplet open
            ((2) (d-PutClipObj staff count))    ;tuplet close
            ((5) (d-PutClipObj staff count))    ;keysignature
            ((4) (d-PutClipObj staff count))    ;timesignature
            ((3) (d-PutClipObj staff count))    ;clef                       
            ((7) (d-PutClipObj staff count))    ;stem-directive
            ((9) #f) ; staffbreak           
            ((#f) #f) ; No object left. Means "no clipboard", too.
            (else (begin (display "Error! Object to paste unknown or unsupported\n") #f))))
            
    ;body
    (d-UnsetMark)
    (set! staffcountlist (iota paste:howmanystaffs))    
    (if paste:multistaff? ; check if the staff-length of all participating staffs is equal. If not append measures.
        (let ()
        (define position:current (1+ position:startmeasure))
        (d-PushPosition)
        (for-each (lambda (x) 
            (if (d-GoToPosition #f (+ position:startstaff x) position:current 1)
                #t
                (begin ; fill in measures up to nr. position:startmeasure
                    (d-GoToPosition #f (+ position:startstaff x) 1 1)
                    (d-MoveToEnd)
                    (Repeat d-AppendMeasure (- position:current (d-GetMeasure)))))) staffcountlist) 
        (d-PopPosition)))   
    (if paste:multistaff?  ; check if the current measure in all needed staffs is empty. If not create an empty measure to start.
        (if (any not (map (lambda (x) (ProbePosition None? #f (+ position:startstaff x) position:startmeasure 1)) staffcountlist))
            (MeasureBreak!)
            (set! position:startmeasure (1- position:startmeasure)))) 
    ;Do the first staff. It will stop on staffbreak or end of the clipboard.    
    (RepeatUntilFail (lambda () (1+count!) (paste! staff count)))
    (if paste:multistaff? 
        (let ()
            (define position:return (GetPosition))          
            (Repeat  ; repeat single-staff pasting for each staff > 0.  
                (lambda ()                  
                    (1+staff!)
                    (set! count -1)
                    (if (d-GoToPosition #f (+ staff position:startstaff) (1+ position:startmeasure) 1) ; if a staff down, go there. else abort.
                        (RepeatUntilFail (lambda () (1+count!) (paste! staff count)))                           
                        "No staff left to paste to. But the beginning of the clipboard was pasted, which is probably what you wanted."))
                paste:howmanystaffs)
            (apply d-GoToPosition position:return))))   
  (if (d-GetClipObjType 0 0) (Paste)))
  
; A copy variant in Scheme
;; Save the selection in a scheme variable
;; Music? are musobj (CreateMusObj)
;;TODO: SchemeCopy and Paste are very limited and need improvement.
(define (SchemeCopy)
  ;If on an end-tuplet marker it gives you the startvalue
  (define (GetTupletFromEndTuplet)
        (let ()
        (define return #f)
            (if (equal? (d-GetType) "TUPCLOSE")
            (begin
                (d-PushPosition)
                (let loop ()
                (if (d-MoveCursorLeft)
                    (if (equal? (d-GetType) "TUPOPEN")
                        (set! return (d-GetTuplet))
                        (loop))
                    #f)) ; staff beginning      
                (d-PopPosition)))
                return))  
  ;Mainfunction to gather data.
  (define (gather)
    (ActionChooser 
        (lambda () (CreateMusObj)) ;chords, notes, rests
        (lambda () (cons 'TUPCLOSE (GetTupletFromEndTuplet))) ; tuplet close
        (lambda () (cons 'TUPOPEN (d-GetTuplet))) ; tuplet open
        (lambda () (disp "lily")) ; lilypond directive
        (lambda () (disp "clef")) ; clefs
        (lambda () (disp "time")) ; timesignatures
        (lambda () (disp "key")) ; keysignatures
        (lambda () (disp "stem")))) ; stem directives /voice presets        
  (if (d-MarkStatus)
    (MapToSelection gather)     
    #f))
    
(define (ProcessSchemeCopyBufferMusObj musobjproc copybuffer)
    ;modify the current musobj and then return the complete, altered, object for the map-list.
    (map (lambda (current) 
        (if (musobj? current) 
            (begin (musobjproc current) current) ; if museobj use the proc
            current)) ; if not just return the original object
         copybuffer))
    
;Paste a list created by (SchemeCopy)
(define (SchemePaste listy)
  (define (insert x)
    (cond
    ((musobj? x)  (ANS::InsertNotes (musobj.pitch x) (musobj.baseduration x) (musobj.dots x)))
    ((equal? (car x) 'TUPCLOSE) (d-EndTuplet))
    ((equal? (car x) 'TUPOPEN) (begin (d-StartTriplet) (d-SetTuplet (cdr x))))
    
    ))
    (for-each (lambda (x) (insert x)) listy))

;Apply the passed script to each movement of a score
(define (ForAllMovements script)
  (d-PushPosition)
  (d-GoToPosition 1 1 1 1)
  (let loop ()
    (begin
      (eval-string script)
      (if (d-NextMovement)
      (loop))))
  (d-PopPosition))

;Apply the passed script to each staff of a movement
(define (ForAllStaffs script)
  (d-PushPosition)
  (d-GoToPosition #f 1 1 1)
  (let loop ()
    (begin
      (eval-string script)
      (if (d-MoveToStaffDown)
      (loop))))
  (d-PopPosition))
  
;Execute the passed procedure on each movement of a score
(define (ForAllMovementsExecute proc)
  (d-PushPosition)
  (d-GoToPosition 1 1 1 1)
  (let loop ()
    (begin
      (proc)
      (if (d-NextMovement)
      (loop))))
  (d-PopPosition))
    
;Execute the passed procedure on each staff of a movement
(define (ForAllStaffsExecute proc)
  (d-PushPosition)
  (d-GoToPosition #f 1 1 1)
  (let loop ()
    (begin
      (proc)
      (if (d-MoveToStaffDown)
      (loop))))
  (d-PopPosition))
  
;Execute the passed procedure on each object of a staff
(define (ForAllObjectsInStaffExecute proc)
  (d-PushPosition)
  (d-GoToPosition #f #f 1 1)
  (let loop ()
    (begin
      (proc)
      (if (d-NextObject)
      (loop))))
  (d-PopPosition))
  
;Execute the passed procedure on each object of a score
(define (ForAllObjectsInScoreExecute proc)
    (ForAllMovementsExecute (lambda () 
        (ForAllStaffsExecute (lambda () 
            (ForAllObjectsInStaffExecute (lambda () 
                (proc))))))))

;Execute the passed procedure on each note in at the cursor
(define (ForAllNotesInChordExecute proc) 
  (if (d-CursorToNthNoteHeight 1)
    (begin
        (proc)
        (while (d-CursorToNextNoteHeight)
            (proc)))))

;;;
(define CreateScriptForDirective::clipboard #f)
(define* (CreateScriptForDirective #:optional (tag #f))
    (define copied #f)
    (define note #f)
    (if (Music?)
        (begin
            (if (not tag)
                (set! tag (d-ChooseTagAtCursor)))
            (if tag
                (begin
                    (set! note (cdr tag))
                    (set! tag (car tag))
                    (set! copied tag)
                    (set! CreateScriptForDirective::clipboard (d-GetScriptForDirective tag note))))))
                    
  (if (and (not copied) (not (MeasureEnd?)))
      (begin
            (d-SetMark)
            (set! copied (string-append (_ "Object of type ") (d-GetType)))))
                        
  (if copied
            (begin
            (d-PlayMidiNote 39 255 9 200)
            (Help::TimedNotice (string-append  "<span font_desc=\"16\" foreground=\"blue\">" (_ "Copied ") copied "</span>"))))) 
        
(define (SelectStaff)
    (d-PushPosition)
    (d-GoToBeginning)
    (d-SetMark)
    (d-GoToEnd)
    (d-SetPoint)
    (d-PopPosition))
