(use-modules (ice-9 optargs)) ; optional (define* ) arguments

#!(define-module (actions denemo-modules rhythmandmeter)
    #:export (
        GetPrevailingTimeSig
        GetMeasureTicks
        EmptyMeasure?
        UnderfullMeasure?
        FullDurationMeasure?
        OverfullMeasure?
        MeasureComplete?
        ChangeDuration value
        duration::GuessBaseNoteInTicks
        duration::CalculateTicksWithDots
        duration::CalculateDotsFromTicks
        duration::CalculateDotsFromTicks
        duration::GetNumberOfDotsInDenemo
        duration::denemo->lilypond
        duration::lilypond->denemo
        duration::denemo->ticks
        duration::lilypond->ticks
        duration::ticks->denemo
        duration::ticks->lilypond
        duration::SplitTicksToBaseDurations
        duration::GetNumberOfDotsInTicks
        duration::GetNumberOfDotsInTicks
        duration::GetNumberOfDotsInTicks
        duration::ChangeNoteDurationInTicks
        duration::InsertBaseDurationList
        duration::GetNumerator
        duration::GetDenominator
        duration::GetDenominatorInTicks
        duration::GetWholeMeasureInTicks
        duration::GetMetricalPosition
        duration::MetricalMain?
        ;d-GetStartTick
        )) !#

(define (ChangeDuration value)
    (case value
        ((0) (d-ChangeTo0))
        ((1) (d-ChangeTo1))
        ((2) (d-ChangeTo2))
        ((3) (d-ChangeTo3))
        ((4) (d-ChangeTo4))
        ((5) (d-ChangeTo5))
        ((6) (d-ChangeTo6))
        ((7) (d-ChangeTo7))))
        
(define* (GetPrevailingTimeSig #:optional (numberorstring #f) ) 
    (if numberorstring
        (string->number (d-GetPrevailingTimesig))
        (d-GetPrevailingTimesig)))
(define GetPrevailingTimesig GetPrevailingTimeSig)
;(define (d-GetStartTick)
;   (- (d-GetEndTick) (d-GetDurationInTicks)))

;Get Measure Filling Status in Ticks
(define (GetMeasureTicks)
    (let script ((return 0))
    (d-PushPosition)
    (GoToMeasureEnd)
    (set! return (d-GetEndTick))

      (d-PopPosition)
      return))
      
;;;the tick count at the end of the object at the cursor
(define (GetTickCount)
    (let ((tick (d-GetEndTick)))
        (if tick
            tick
            0)))

(define (MeasureFillStatus)
    (define MaxTicks (* 1536 (GetPrevailingTimeSig #t) )) ; How many ticks are in a 100% filled measure?
    (define ActualTicks (GetMeasureTicks))
    (cond 
        ((not ActualTicks) #f) ; empty
        ((< ActualTicks MaxTicks) #f) ; underful
        ((= MaxTicks ActualTicks) 1)  ; 100% filled
        ((< MaxTicks ActualTicks) 2) ; >100% filled
        (else  (display "strange!")))) ; ?
    

(define (EmptyMeasure?)
  (not (d-GetEndTick)))

(define (ZeroDurationMeasure?)
    (or (EmptyMeasure?) (= (GetMeasureTicks) 0)))
  
(define (UnderfullMeasure?)
  (or (EmptyMeasure?)
     (not (MeasureFillStatus))))

(define (FullDurationMeasure?)
  (and (not (UnderfullMeasure?))
       (= 1 (MeasureFillStatus))))

(define (OverfullMeasure?)
  (eq? 2 (MeasureFillStatus)))
  ;;(and (not (EmptyMeasure?)) 
 ;; (> (d-GetEndTick) (* 1536 (GetPrevailingTimeSig #t)))))

(define (MeasureComplete?)
    (d-RefreshCache)
    (FullDurationMeasure?))
    
(define (AcceptableDurationMeasure?)
    (if (d-MoveToMeasureRight)
        (begin
            (d-MoveToMeasureLeft)
            (or (ZeroDurationMeasure?) (FullDurationMeasure?) (d-Directive-standalone? "!")))
        #t))
        
(define (NextMeasureEmpty?)
    (define return #f)
    (d-PushPosition)
    (set! return (and (d-MoveToMeasureRight) (EmptyMeasure?)))
    (d-PopPosition)
    return)
        
(define (EnsureEmptyNextMeasure all)
 (d-PushPosition)
 (if (not (NextMeasureEmpty?))
    (if all
        (d-AddMeasure)
        (d-InsertMeasureAfter)))
 (d-PopPosition))
                   
;;;;;;;;;;Duration Conversions between Denemo, Lilypond and Tick syntax.
;; A table of common values
; 6 = 256 = 8
;12 = 128 = 7
;24 = 64 = 6
;48 = 32 = 5
;96 = 16 = 4
;192 = 8 = 3
;384 =4 = 2
;768 = 2 = 1
;1536 = 1 = 0
;3072 = 0.5  = -1  ; Breve. 0.5 and -1 are not existend.  Lilypond and Denemo use the string breve instead.

; Guile returns a value with .0, which should be exact but internally it's inexact. So we need this little back and forth conversion hack.
;(inexact->exact (string->number (number->string return))))
(define (duration::inexact->exact return)
    (define val (if return (string->number (number->string return)) 1))
   ;(disp "Working with parameter " return " and val has become " val "which is " (exact? val) (inexact? val) "\n\n")
    (if (inexact? val)
        (set! val (truncate val)))
    (if (< return 0)
        0
        (if (inexact? val)
            (begin ;(disp "returning" (inexact->exact val) "\n\n") 
                (inexact->exact val))
            val)))

;Some functions, like Upbeat, know only a single tick-value. They need to guess the baseNote.
(define (duration::GuessBaseNoteInTicks ticks)
; first guess the basic note duration.  2*x > ticks < 1*x  is always true in our circumstance 
  (cond
     ( (and (>= ticks 6) (< ticks 12)) 6 )       ;1/256
     ( (and (>= ticks 12) (< ticks 24))  12 )   ;1/128
     ( (and (>= ticks 24) (< ticks 48)) 24)   ;1/64
     ( (and (>= ticks 48) (< ticks 96)) 48 )   ;1/32
     ( (and (>= ticks 96) (< ticks 192))  96)   ;sixteen 1/16
     ( (and (>= ticks 192) (< ticks 384)) 192 ) ; eight 1/8
     ( (and (>= ticks 384) (< ticks 768))  384 ) ; quarter 1/4
     ( (and (>= ticks 768) (< ticks 1536))  768) ; half 1/2
     ( (and (>= ticks 1536) (< ticks 3072))  1536 ) ; whole 1
     ( (and (>= ticks 3072) (< ticks 6144))  3072 ) ; breve 2*1
     ( (and (>= ticks 6144) (< ticks 12288))  6144) ; longa 4*1
     ( (and (>= ticks 12288) (< ticks 24576))  12288 )  ; maxima 8*1
    (else #f)))

; Calculate a new Duration in Ticks with a basic note value and a number of augmentation-dots
(define (duration::CalculateTicksWithDots baseTicks numberOfDots)
    ; x = basic note value
    ; n = number of dots
    ; 2x - x/2^n 
    (-  (* 2 baseTicks) (/ baseTicks (expt 2 numberOfDots))))

; Calculate how many dots a tick value has. Needs base duration, too.
(define (duration::CalculateDotsFromTicks ticks base)
; x = base , y = ticks. result is the number of dots
; log(x/(2*x-y))  / log(2)
 (define return (/  (log (/ base (- (* base 2) ticks)))   (log 2) ))
 (duration::inexact->exact return))

; Get Number of Dots from a Lilypond string like "2.". Its so basic it will work on Denemo notes, too.
(define (duration::GetNumberOfDotsInLilypond input)
 (length (cdr (string-split input #\.))))

; For the sake of completenes. Denemo and Lilypond dot-syntax is just the same, only the number itself is different.
(define (duration::GetNumberOfDotsInDenemo input)
    (duration::GetNumberOfDotsInLilypond input))

(define (duration::denemo->lilypond number)
    (define return (expt 2 number))
    return)

(define (duration::lilypond->denemo number)
    (define return (/ (log number) (log 2)) )
     (duration::inexact->exact return))

(define (duration::denemo->ticks number) ; increases with negative integers
    (define return (* (expt 2 (- 8 number)) 6))
    return)

(define (duration::lilypond->ticks number) ; increases with 0.5, 0.25 etc.
    (define return (* (expt 2 (- 8 (/ (log number) (log 2)))) 6))
     (duration::inexact->exact return))

;;returns a string for the shortfall in the current measure. has problems with exact/inexact stuff...
(define (duration::shortfall)
	(define ticks  (if (GetMeasureTicks) (GetMeasureTicks) 0))
    (define guess (duration::GuessBaseNoteInTicks  (-  (* 1536 (GetPrevailingTimeSig #t)) ticks)))
    (if guess
    (duration::ticks->denemo 
        (-  (* 1536 (GetPrevailingTimeSig #t)) ticks) guess)
    #f))
;;;this simple version works for say a crotchet upbeat...
;(define (duration::shortfall)
;   (duration::ticks->denemo 
;   (-  (* 1536 (GetPrevailingTimeSig #t))   (GetMeasureTicks))
;   (duration::GuessBaseNoteInTicks  (-  (* 1536 (GetPrevailingTimeSig #t))   (GetMeasureTicks)))))






; Ticks->Denemo wants a number but returns a string because of dots
(define* (duration::ticks->denemo number #:optional (basenumber number))
 (define numberOfDots  (duration::CalculateDotsFromTicks number basenumber))
;n = -(log(y/3)-9*log(2))/log(2) 
 (define return (- (/ (- (log (/ basenumber 3)) (* 9 (log 2))) (log 2)))) 
 (set! return (duration::inexact->exact return))
 (string-append (number->string return) (string-concatenate (make-list numberOfDots "."))))

;Ticks->Lilypond wants a number but returns a string because of dots
(define* (duration::ticks->lilypond number #:optional (basenumber number))
 ;Equation in readable form: http://www.numberempire.com/equationsolver.php?function=y+%3D+6*2^%288-n%29&var=n&answers=
 (define numberOfDots  (duration::CalculateDotsFromTicks number basenumber))
 (define return  (expt 2 (- (/ (- (log (/ basenumber 3)) (* 9 (log 2))) (log 2)))))
 (set! return (duration::inexact->exact return))
 (string-append (number->string return) (string-concatenate (make-list numberOfDots "."))))

;SplitTicksToBaseDurations wants a single tick value as number and splits it to the lowest count of base durations
;;Returns a list of base durations
;;TODO: if the last step is no BaseDuration return it anyway and an additional #f to signal the problem. Most likely someone tried to involve an incomplete tuplet.
;;TODO: Basic version. Duration will be correct but maybe ugly.
(define (duration::SplitTicksToBaseDurations ticks)
(let loop ((number ticks) (returnlist (list #f)))
    (define basedur (duration::GuessBaseNoteInTicks number))    
    (if basedur
        (loop (- number basedur) (append returnlist (list basedur)))
        (if (= number 0)
            (list-tail returnlist 1)
            (list-tail (append returnlist (list number #f)) 1)))))           

;;;;;;;;;; End of duration-conversion

;; Applied duration scripts
(define (duration::GetNumberOfDotsInTicks) ; Fails for tuplets
     (duration::CalculateDotsFromTicks (d-GetDurationInTicks) (duration::GetBaseDurationInTicks)))
       

(define (duration::GetBaseDurationInTicks)
    (define ticks (d-GetBaseDurationInTicks))
    (if ticks
        (abs ticks)
        #f))       

(define (duration::GetSelectionDurationInTicks)
    (if (d-MarkStatus)
        (apply + (MapToSelection d-GetDurationInTicks Music?))
        #f))
    

(define* (duration::ChangeNoteDurationInTicks ticks #:optional (dots 0))
; First change the base-duration. The d-Change commands also delete any dots.
  (define (changeBase number) (case number
   ;((12288) (d-ChangeMaxima))  ;Maxima
   ((6144)  (d-ChangeLonga)) ; Longa
   ((3072)  (d-ChangeBreve)) ; Breve
   ((1536)  (d-Change0)) ; Whole
   ((768)   (d-Change1)) ; Half
   ((384)   (d-Change2)) ; Quarter
   ((192)   (d-Change3)) ; Eighth
   ((96)    (d-Change4)) ; Sixteenth
   ((48)    (d-Change5)) ; 1/32
   ((24)    (d-Change6)) ; 1/64
   ((12)    (d-Change7)) ; 1/128
   ((6)     (d-Change8)) ; 1/256
   (else   #f ))) 
; Second step: add dots
  ; d-ChangeN work on appending position, but Breve and Longa not. But d-AddDot works on appending, too. So we must rule Appending out, else it will add dots without changing the duration for breve and longa.
  (if (and (Music?) (integer? ticks) (integer? dots) (changeBase ticks)) ; <-- the action changeBase itself needs to be a test, too. 
  (let loop ((i 0))
    (if (= dots i)
    #t
    (begin
       (d-AddDot)  
       (loop (+ i 1)))))
  #f))

(define (duration::InsertBaseDurationList basedurlist ansNotes)
    (define itemnumber (length basedurlist))
    (let loop ((counter 0))
        (if (and (> counter 0) (= (list-ref basedurlist counter) (/ (list-ref basedurlist (1- counter)) 2)))  ; current duration is half of previous one = current is a dot. First position excluded.
            (begin (d-MoveCursorLeft) (d-AddDot) (d-MoveCursorRight))
            (begin (ANS::InsertNotes ansNotes) (d-MoveCursorLeft) (duration::ChangeNoteDurationInTicks (list-ref basedurlist counter)) (d-ToggleTie)  (d-MoveCursorRight)))     
        (if (>= counter (1- itemnumber))
            (begin (d-MoveCursorLeft) (d-ToggleTie) (d-MoveCursorRight) #t) ; last item was already inserted. Undo the last tie. The End.
            (begin (loop (1+ counter))))))  

;; Meter related functions
(define (duration::GetNumerator) (string->number (car (string-split  (GetPrevailingTimeSig) #\/))))
(define (duration::GetDenominator) (string->number (cadr (string-split  (GetPrevailingTimeSig) #\/))))
(define (duration::GetDenominatorInTicks) (* 1536 (/ 1 (duration::GetDenominator))))
(define (duration::GetWholeMeasureInTicks) (* 1536 (GetPrevailingTimeSig #t)))
(define (duration::GetMetricalPosition)
    (if (MeasureEmpty?)
        1
        (1+ (/ (d-GetStartTick) (duration::GetDenominatorInTicks)))))
(define (duration::MetricalMain? position) (integer? position))

