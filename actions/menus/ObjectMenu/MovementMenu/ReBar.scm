;Rebar
;Fixed for tuplets giving inexact numbers of ticks
;Rebar-repartitions the measures, searches for under/overfull, pad with rests, by DW, trivial mods by RTS
;FIXME-currently no support for nested tuplets
;notes: (d-GetType) returns CHORD TIMESIG LILYDIRECTIVE TUPOPEN TUPCLOSE (tuplets) Appending
;Variable descriptions:
;SplitAll set to #f allows user to decide whether to split across barlines; set to a number, splits that many times.
;TupletScaleFactor: e.g. if we're inside a triplet, scale durations by 2/3
;IgnoreDurationError set if a measure is marked as having an allowable duration discrepancy, marking is done with a directive of tag "!"

(define ReBar::return #f)
(define-once CheckScore::error-position #f)

(let ( (Input1 #f) (Input2 #f) (InitialTimeSig 1) (ScanAllStaffs #f) (TupletScaleFactor 1)  
(SplitAll #f)(IgnoreDurationError #f))
    
;MakeBlankRestDuration: makes a tied rest conglomerate of total duration Duration.
;IsConsecutive is set to #t when it recurses in order to make it add a dot instead of adding the next TryDuration.  Externally, call it always set to #f
;externally, call with TryDuration=0 to start with trying out whole notes.  Internally it will crank this up as it recurses.

(define (MakeRest Duration TryDuration Type)  ;set Type to "Blank" to get blanks.
    (begin
        (if (not (equal? TupletScaleFactor 1)) 
            (begin
                (d-InfoDialog (_ "There are unclosed tuplets in this bar.\n Please fix - see CheckTuplets command."))
                #f  ;terminate
            )
        )
        (if (and (> Duration 0) (equal? (remainder 256 (denominator Duration)) 0)  ) ;check for valid Duration
            (begin  ;when Duration is a valid duration...           

                (if  (<= (expt 1/2 TryDuration) Duration)   ;see if we should add note of length TryDuration 
                    (begin
                        (InsertRest TryDuration Type)   
                        (set! Duration (- Duration (* (expt 1/2 TryDuration) TupletScaleFactor)));now modify true Duration by what we just added.
                        
                    )
                )
                (if (<= TryDuration 8) 
                    (MakeRest Duration (+ 1 TryDuration) Type)
                )           
            )
            #f              
        )
    );let
)

(define (InsertRest  Duration Type) ;inserts a rest of duration TryDuration, and makes it tied. denom of Duration must be power of 2.

    (if (equal? Type "Blank") (d-InsertBlankWholeNote)  (d-InsertWholeRest) )   ;Insert correct type
    (if (equal? (d-GetType) "Appending")  (d-MoveCursorLeft))  ;now we're on it. (Don't need to go left if we're not appending)
    (while (> Duration 0) ;we keep diminishing until we got it.
        (begin
            (d-Diminish)
            (set! Duration (- Duration 1))
        )
    )
    ;(d-MoveCursorRight)
)

;SplitIntoNext:splits a measure by inserting a barline, ending in next bar
(define (SplitIntoNext) ;custom version of SplitIntoNext used here
(begin
    (d-SetMark)
    (if   (d-MeasureRight)
        (begin  (d-CursorLeft)  
                    (d-Cut)
                    (d-CursorRight)
                    (d-InsertMeasureBefore)
            )
        (begin
                (d-GoToEnd)
            (d-Cut)
                (d-AppendMeasure)
            (d-MoveToMeasureRight)
        )
    )
    (d-Paste)   ;now, position the cursor at begin of new bar:
    (d-MoveToMeasureLeft)  
    (d-MoveToMeasureRight)
    (d-UnsetMark)
))

(define (CustomDeleteBarline)
(let  ((type (d-GetType)) );don't try it if not at the end of bar
  (if (or (equal? type "Appending" ) (equal? type "None" ) )
    (begin (d-SetMark)
    (if (d-MoveToMeasureRight) ;;we don't want to delete next bar if there isn't one!
    (begin (d-MeasureLeft) ;; Now we're at the beginning of that measure
        (d-Cut)
        (d-DeleteMeasure)
        (d-Paste) )
    (d-UnsetMark) ;this is to clear the mark if we set it, but were at end of staff
)))))

;MakeDuration: makes a tied note conglomerate of total duration Duration.
;set Side to "Left" to have the shorter component durations on left, "Right" for other way.
;note that using "Left will leave the rightmost note tied since we will tie into next bar.
;IsConsecutive is set to #t when it recurses in order to make it add a dot instead of adding the next TryDuration.  Externally, call it always set to #f
;externally, call with TryDuration=0 to start with trying out whole notes.  Internally it will crank this up as it recurses.

(define (MakeDuration Duration Side TryDuration IsConsecutive) 
    (let ((EffectiveDuration 0))
        (set! EffectiveDuration (/ Duration TupletScaleFactor)) ;if inside a tuplet, need to 
                                                    ;account for that by extending the effective duration proportionately.
        (if (and (> Duration 0) (equal? (remainder 256 (denominator EffectiveDuration)) 0)  ) ;check for valid Duration
            (begin  ;when EffectiveDuration is a valid duration...          
                (if  (<= (expt 1/2 TryDuration) EffectiveDuration)  ;see if we should add note of length TryDuration 
                    (begin
                        (if IsConsecutive 
                            (begin
                                (if (equal? Side "Right") (d-MoveCursorLeft));b/c when adding on the Right, we moved the cursor Right already
                                (d-AddDot) ;if previous duration was there, just add a dot.
                                (if (equal? Side "Right") (d-MoveCursorRight));returning it to its former position
                            )
                            (begin  ;if previous duration wasn't...
                                (set! IsConsecutive #t) ;so that next smaller duration will know to just add a dot if it's there
                                (InsertOn Side TryDuration)
                            )
                        )   
                        (set! Duration (- Duration (* (expt 1/2 TryDuration) TupletScaleFactor)));now modify true Duration by what we just added.
                        (if (and (equal? Duration 0) (equal? Side "Right")) ;rightmost note entered on Side=Right, don't leave tied.
                            (begin
                                (d-MoveCursorLeft)
                                (d-ToggleTie)
                                (d-MoveCursorRight)
                            )
                        )
                    )
                    (set! IsConsecutive #f) ;when we don't add this TryDuration, set this to #f.
                )
                (if (<= TryDuration 8) 
                    (MakeDuration Duration Side (+ 1 TryDuration) IsConsecutive)
                )           
            )
            #f  
            
            
        )
    );let
)

(define (InsertOn Side Duration) ;inserts a note of duration TryDuration, and makes it tied. denom of Duration must be power of 2.

    (d-Paste)  ;paste, but then must get on top of the notes just pasted...
    (d-MoveCursorLeft)  ;now we're on it. 
    (d-Change0) ;start with whole, 
    (while (> Duration 0) ;and keep diminishing until we got it.
        (begin
            (d-Diminish)
            (set! Duration (- Duration 1))
        )
    )
    (d-ToggleTie)
    (if (equal? Side "Right") (d-MoveCursorRight) )
)


;Now here's the function, it returns #f if there is a problem in a measure in the current staff that is not fixed else returns true

(define (AuditThisStaff TimeSig Pad? Blank? MergeAndSplit?) ;;set Pad? to #t if we should add blanks to underfull bars. 
    (let ( (Counter 0) (Excess 0) (LeftOver 0)(Inquiry #f))
        ;Counter keeps track of the duration of the notes of the bar as we proceed chord by chord.  
        
        ;Debugger can be useful for bug-fixing
        (define (Debugger)
            (begin (d-RefreshDisplay)
            (if (not (d-GetUserInput "Debug Message" 
                (string-append "Counter:" (number->string Counter) "\nTupletScaleFactor:" (number->string TupletScaleFactor) "\nTimeSig:" (number->string
                    TimeSig)
                 ) "Hit OK when ready" ) ) (#f) ) 
        ))
        
        (define (GetTimeSigChange)
            (set! TimeSig (GetPrevailingTimesig #t))
        );GetTimeSig
        

            
        (define (NextBreakInMeasure) ;move to next object in measure, skipping objects of no duration, return #t if we found something with nonzero duration
            (let ((GNB 0)(Moved #f))
                (while 
                    (and 
                        (d-NextObjectInMeasure) 
                        (set! Moved #t) ;make sure we know we moved
                        (equal? (GetNoteBeat) 0)    ;do this so we don't run GetNoteBeat twice on same object (it could mess up counts?)

                    )
                )
                (and Moved (not (equal? (GetNoteBeat) 0)))  ;return #t if we Moved and found something with nonzero duration
            )
        )
        
        (define (GetNoteBeat )  ;get duration of a note as a fraction of a whole note, e.g. dotted quarter = 3/8
            (let ((note 0) (len 0 ) (DotIndex 0) (NumDots 0) (NoteBeat 0))
                (begin
                (if (equal? (d-GetType) "TIMESIG") 
                    (if (equal? Counter 0) ;encountering a TimeSig change mid-measure requires user intervention
                        (GetTimeSigChange)
                        (begin
                            (d-InfoDialog "This time signature change is in the middle of the bar.\n
                            Please run the command again after you've fixed this.")
                            (set! NoteBeat #f) ;this should halt the script after it's returned.
                        )
                    )
                )
                
                (if (equal? (d-GetType) "LILYDIRECTIVE")
                    (begin
                        (set! NoteBeat (/ (d-GetDurationInTicks) 1536))
                        (set! IgnoreDurationError (d-Directive-standalone? "!"))) ;;This tag tells Denemo to ignore duration errors in this measure.
                )
                (if (equal? (d-GetType) "CHORD" ) 
                    (if (not (d-ToggleGrace "query="))  ;if it's not a grace, continue; otherwise, leave it as 0.                   
                            (set! NoteBeat (/ (d-GetDurationInTicks) 1536))                 
                    )
                ) 
                 NoteBeat   ;return NoteBeat
                )
            )
        );GetNoteBeat
        (define (R-StartTuplet TupletFactor)    ;note: doesn't modify TupletScaleFactor.
            (begin
                (d-StartTriplet)
                (if (boolean? (d-GetTuplet))
                (d-MoveCursorLeft))
                (d-SetTuplet  (number->string TupletFactor ) )
                (d-MoveCursorRight)
            )
        )

        (define (LoopThroughBar)   ;stops once we've met or surpassed the measure size, or run out of new notes.
            (if (and (< Counter TimeSig) (d-NextObjectInMeasure) )  ;as long as the Counter is less than a full bar, and there's more stuff to process...
                (begin
                    (set! Counter (+ Counter (GetNoteBeat)) )   ;we increment the Counter,
                    (LoopThroughBar)    ;and keep going until done with the bar.
                    
                )
            )
            (set! CheckScore::error-position (GetPosition))
        )
        ;here's the actual algorithm
        (set! IgnoreDurationError #f)
        (while (d-PrevObjectInMeasure)) ;go to beginning of measure
        (set! Counter (+ Counter (GetNoteBeat)) );read the first note in to get started...NOTE: if GetNoteBeat= #f this will terminate execution.
        (LoopThroughBar)    ;then loop through the rest of the bar until counter equals or overshoots the measure size in TimeSig,
                        ; or the measure's done being processed
            ;(disp "check Counter " Counter " and time sig " TimeSig "and " IgnoreDurationError " ok")
            ;;ticks have granularity of 1 so we cannot accept a 1 discrepancy as meaning anything once we have reached a certain number of ticks - how many I am not sure, but try 383, fails with septuplet, try 255
        (if (and (None?) (not Pad?))
            (set! Counter TimeSig))
            
         (if (= Counter 0)
                (set! Counter TimeSig));;;allow empty measures
            
            
        (if IgnoreDurationError
            (begin
                (set! Counter TimeSig)))
                
        (let ((top (numerator Counter)) (bottom (denominator Counter)))
            ;(disp "We have top " top " bottom " bottom " div "  (/ (1+ top) bottom) " ok")
            (if  (and (> top 254) (equal? TimeSig (/ (1+ top) bottom)))
                (set! Counter (/ (1+ top) bottom))))
                
                    ;(disp "Set Counter " Counter "\n")          
        (if (< Counter TimeSig) ;if measure too small, (going back first)
            (if Pad?    ; and the user wants us to pad,
                (begin
                    (if (not (or (equal? (d-GetType) "None") (equal? (d-GetType) "Appending"))) 
                        (d-MoveCursorRight) )   ;Get to end of bar, past last object.
                    (if Blank?
                        (MakeRest (- TimeSig Counter) 0 "Blank")    ;fill 'er up
                        (MakeRest (- TimeSig Counter) 0 "" )
                    )
                    (if (d-MoveToMeasureRight)
                        (AuditThisStaff TimeSig Pad? Blank? MergeAndSplit?) ;if not done, move on and go from there.
                        #t      ;otherwise return true
                    )
                )   
                (if MergeAndSplit?  ;if user wants to merge/split,
                    (if (d-MoveToMeasureRight)  ;we see if there is a next measure; if so we want to merge the two and redo this measure.
                        (begin
                            (d-MoveCursorLeft) ;move onto the barline 
                            (CustomDeleteBarline) ;delete it
                            (AuditThisStaff TimeSig Pad? Blank? MergeAndSplit?);now do the measure again 
                        )
                        #t  ;return true if there's no more, we're done with staff.
                    )
                    ;if NOT supposed to merge/split...
                    (if (d-MoveToMeasureRight)  ;see if at end of staff.
                        (begin              ;if not at end of staff...
                            (d-MoveToMeasureLeft)
                            #f  ;if we're not supposed to pad or merge/split, stop here.  This measure had prob's
                        )
                        #t      ;otherwise, staff is good.
                    )))
                    ;if it wasn't too small...
            (if (and (not IgnoreDurationError)  (equal? Counter TimeSig)) ; and if measure is exactly full now (and not just because we are ignoring the duration error)
                (if (NextBreakInMeasure) ;see if there's extra stuff that has duration,
                    (if MergeAndSplit?  ;if there IS and we're supposed to merge/split...
                        (begin
                            (if (not (= TupletScaleFactor 1)) (d-EndTuplet)) ;if need be, end the tuplet in this bar, then restart it in next
                            (SplitIntoNext) ;cut off extra stuff (ending up in next bar)
                            (if (not (= TupletScaleFactor 1)) (R-StartTuplet TupletScaleFactor)) ;restart the tuplet in following bar if needed
                            (AuditThisStaff TimeSig Pad? Blank? MergeAndSplit?)     ; rebar from that point onward.                     
                        )
                        #f  ;if user doesn't want us to split,return false.
                            ; this bar is over-full, user will fix.
                    )
                    (begin  ;if there's NOT extra stuff in this measure...
                        (if (not (None?))
                            (d-MoveCursorRight))
                        (if (not (= TupletScaleFactor 1)) (d-EndTuplet)) ;if need be, end the tuplet in this bar, then restart it in next

                        (if (d-MoveToMeasureRight) ;if there's another measure...
                            (begin 

                                (if (not (= TupletScaleFactor 1)) (R-StartTuplet TupletScaleFactor)) ;restart the tuplet in following bar if needed
                                (AuditThisStaff TimeSig Pad? Blank? MergeAndSplit?);having gone on to next measure, rebar again.
                            )
                            #t  ;make sure we return true-done with staff.                          
                        )
                    )
                )
                    ;;;measure not exact or we are ignoring the duration error
                (if (and MergeAndSplit? (> Counter TimeSig)) ; if the measure is overfull, see if want to split.
                    (begin
                        (set! Excess (- Counter TimeSig)) ;this is how much of that note to put in next measure,                        
                        (if (equal? (remainder 256 (denominator (/ Excess TupletScaleFactor))) 0)  ;if we don't have to start a tuplet, we're good.
                            (begin
                                ;query the user: should we split the note, or let him/her do it?                
                                (if (not SplitAll) (set! Inquiry (d-GetOption (string-append (_ "Split This Note") stop (_ "Split Many") stop (_ "Stop Here") stop))) )
                                ;need to stop if we hit cancel
                                (if (equal? Inquiry (_ "Split Many")) (set! SplitAll 100))
                                (if (or SplitAll (equal? Inquiry (_ "Split This Note")) (equal? Inquiry (_ "Split Many"))) 
                                    (begin  ;we're going to split across the barline and march on.
										(if SplitAll
											(begin
												(set! SplitAll (1- SplitAll))
												(if (zero? SplitAll)
													(set! SplitAll #f))))
                                        (set! LeftOver (- (GetNoteBeat) Excess)) ;duration that stays in left measure.      
                                        (if (d-NextObjectInMeasure) ;if there're more stuff after the current stuff, chop it off to deal with it next bar
                                            (begin
                                                (SplitIntoNext)
                                                ;now go back onto the note that should be split by barline.
                                                (d-MoveToMeasureLeft)
                                                (d-MoveToMeasureRight)
                                                (d-MoveCursorLeft)  ;now on barline.
                                                (d-MoveCursorLeft) ;now we're back on the last note of that bar.
                                            )
                                        )
                                        ;now we gotta get a copy of the note to split.
                                        (d-SetMark)
                                        (d-Cut) ;now the right note's cut onto the clipboard...
                                        (MakeDuration LeftOver "Left" 0 #f) ;syntax to add custom duration to each measure, shortest durations to left.
                                        (if (not (= TupletScaleFactor 1))  ;end the tuplets in this bar if need be, restart in next...
                                            (begin (d-MoveCursorRight) (d-EndTuplet)) )                         
                                        (if (not (d-MoveToMeasureRight)) (d-InsertMeasureAfter))    ;need to add a new measure if it ain't there.
                                        (if (not (= TupletScaleFactor 1)) ;start the tuplets in next bar if need be.
                                            (R-StartTuplet TupletScaleFactor) )
                                        (MakeDuration Excess "Right" 0 #f)  ;add with shortest durations on right
                                        (AuditThisStaff TimeSig Pad? Blank? MergeAndSplit?) ;continue                           
                                    )
                                    #f ;if user cancelled or Stopped Here, return false
                                )
                            )
                            (begin
                                (d-InfoDialog (_ "This measure's discrepancy requires a tuplet.\nPlease adjust manually and run this script again."))
                                #f  ;return false
                            )
                        )                   
                    )
                    ;;; measure is not exact and merging is not asked for
                    IgnoreDurationError  ;just return #f as they don't want to Merge/Split, unless it is to be ignored
                )
            )
        )
    )
) ;define AuditThisStaff

;now actually do it:
(if ReBar::params
   (begin
     (d-MoveToBeginning) 
     (set! ReBar::return (AuditThisStaff (GetPrevailingTimesig #t) #f #f #f))
    )
    (let ((Pad #f) (Blank #f)(MergeAndSplit #f))
    (set! Input1 (d-GetOption (string-append (_ "Search for under/overfull bars") stop 
                (_ "Pad underfull bars with rests") stop (_ "Pad underfull bars with blank rests")  stop
                (_ "Rebar-Merge underfull, split overfull bars") stop)))
    (set! Blank (equal? Input1 (_ "Pad underfull bars with blank rests")))
    (set! Pad (or (equal? Input1 (_ "Pad underfull bars with rests"))(equal? Input1 (_ "Pad underfull bars with blank rests") )))
    (set! MergeAndSplit (equal? Input1 (_ "Rebar-Merge underfull, split overfull bars")))
    (if Input1 (set! Input2  (d-GetOption (string-append  (_ "Entire Staff") stop (_ "This Point Onwards")  stop (_ "Entire Movement") stop ))) )
    (if (and Input1 Input2)  ;don't go if user cancelled
        (let ((position #f)(spillover (d-GetBooleanPref "spillover")))
            (d-SetPrefs "<spillover>0</spillover>")
            (if (equal? Input2 (_ "Entire Movement")) (set! ScanAllStaffs #t))
            (set! position (GetPosition))   ;let's try to return cursor to here when done.
            (if ScanAllStaffs (while (d-MoveToStaffUp)))    ;Start at top staff, top voice
            (if (not (equal? Input2 (_ "This Point Onwards"))) (d-MoveToBeginning))
            (set! InitialTimeSig (GetPrevailingTimesig #t))
            
            (let ((AllOK #t))
                (if (not (AuditThisStaff InitialTimeSig Pad Blank MergeAndSplit)) (set! AllOK #f))
                (while (and ScanAllStaffs AllOK (or (d-MoveToVoiceDown) (d-MoveToStaffDown))) ;RebarAll if appropriate.
                    (begin
                        (d-MoveToBeginning)
                        (if (not (AuditThisStaff  InitialTimeSig Pad Blank MergeAndSplit) ) (set! AllOK #f))    ;if AuditThisStaff returns #f, need to stop.
                        )
                    )
                (set! ReBar::return AllOK)
                (if AllOK 
                    (apply d-GoToPosition position) ;end where we began, unless there were problems.
                    ))
            (if spillover (d-SetPrefs "<spillover>1</spillover>"))        
                    
                    
                    )))))   ;let

(if ReBar::params
    (begin ;; non-interactive
        (if ReBar::return
            (set! ReBar::return #f)
            (set! ReBar::return (_ "Problem with measure length")))))
    
