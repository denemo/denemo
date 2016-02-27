; A set of simple tests / questions for score objects. 

;;; Not Appending
(define (Music?) 
  (string=? (d-GetType) "CHORD"))
    
(define (Note?) 
  (and (Music?) (d-GetNoteName)))

(define (Rest?)
   (and (not (d-GetNoteName)) (Music?)))
(define (AtMusic?) 
  (string=? (d-GetType #f) "CHORD"))

;;;On or Appending after    
(define (AtNote?) 
  (and (AtMusic?) (d-GetNoteName)))

(define (AtRest?)
   (and (not (d-GetNoteName)) (AtMusic?)))
    

(define (Chord?) 
  (if (Note?)
    (string-contains (d-GetNotes) " ")
    #f)) ; no note
    
(define (AtChord?) 
  (if (Note?)
    (string-contains (d-GetNotes) " ")
    #f)) ; no note

(define (SingleNote?) 
  (if (Note?)
    (not (string-contains (d-GetNotes) " "))
    #f)) ; no note
 
(define (AtSingleNote?) 
  (if (AtNote?)
    (not (string-contains (d-GetNotes) " "))
    #f)) ; no note 
 
    
(define (Directive?) 
  (string=? (d-GetType) "LILYDIRECTIVE"))

(define (Timesignature?) 
  (string=? (d-GetType) "TIMESIG"))
  
(define (Keysignature?) 
  (string=? (d-GetType) "KEYSIG"))
  
(define (Clef?) 
  (string=? (d-GetType) "CLEF"))
  


(define (TupletOpen?) 
  (string=? (d-GetType) "TUPOPEN"))
  
(define (TupletClose?) 
  (string=? (d-GetType) "TUPCLOSE"))
  
(define (TupletMarker?) 
  (or (TupletOpen?) (TupletClose?)))
 
;;; types on or after when appending
  
(define (AtDirective?) 
  (string=? (d-GetType #f) "LILYDIRECTIVE"))

(define (AtTimesignature?) 
  (string=? (d-GetType #f) "TIMESIG"))
  
(define (AtKeysignature?) 
  (string=? (d-GetType #f) "KEYSIG"))
  
(define (AtClef?) 
  (string=? (d-GetType #f) "CLEF"))

(define (AtTupletOpen?) 
  (string=? (d-GetType #f) "TUPOPEN"))
  
(define (AtTupletClose?) 
  (string=? (d-GetType #f) "TUPCLOSE"))
  
 (define (AtTupletMarker?) 
  (or (AtTupletOpen?) (AtTupletClose?)))
 

(define (Tupletopen?) ;deprecated name  
  (TupletOpen?))
  
(define (Tupletclose?) ;deprecated name  
  (TupletClose))
  
(define (Tupletmarker?) ;deprecated name  
    (TupletMarker?))
  
(define (Singlenote?) ;deprecated name  
    (SingleNote?))


(define (StemDirective?) 
  (string=? (d-GetType) "STEMDIRECTIVE"))  

 
(define (None?)
 (string=? (d-GetType) "None"))
 
(define (MeasureEmpty?) (None?)) 

(define (MovementEmpty?) (and (None?) (= 1 (d-GetMeasuresInStaff)) (= 1 (d-GetStaffsInMovement))))
    
(define (Appending?)
 (string=? (d-GetType) "Appending"))  

(define (MeasureEnd?)
    (or (Appending?) (MeasureEmpty?)))

(define (MeasureBeginning?)
    (= 1 (d-GetHorizontalPosition)))
    
(define  (ColumnEmpty?)
    (define return #f)
    (define measure (d-GetMeasure)) ; to make shure we stay in the same column all the time.
    (d-PushPosition)
    (if (not (MoveToColumnStart))
      #f ; if we have unequal staff length in staff 1 stop immediatly, 
      (let loop ()      
        (if (and (None?) (equal? measure (d-GetMeasure)))
            (begin
                (set! return #t)
                (if (d-MoveToStaffDown)
                    (loop)))        
            (set! return #f))))
    (d-PopPosition)
    return)
    
;ActionChooser is a meta function to provide a simple way to react to all types of Denemo items in the score.
(define (ActionChooser chord tupclose tupopen lilydirective clef timesig keysig stemdirective)  
    (define type (string->symbol (d-GetType)))
    (case type
        ((CHORD) (chord))
        ((TUPCLOSE) (tupclose))
        ((TUPOPEN)  (tupopen))
        ((LILYDIRECTIVE) (lilydirective))
        ((CLEF) (clef)) 
        ((TIMESIG) (timesig))
        ((KEYSIG) (keysig))
        ((STEMDIRECTIVE) (stemdirective))
        (else #f)))

            #! (define (ActionChooserExample)
                  (ActionChooser 
                    (lambda () (disp "Chord")) ;chords, notes, rests
                    (lambda () (disp "tupclose")) ; tuplet close
                    (lambda () (disp "Tupopen")) ; tuplet open
                    (lambda () (disp "lily")) ; lilypond directive
                    (lambda () (disp "clef")) ; clefs
                    (lambda () (disp "time")) ; timesignatures
                    (lambda () (disp "key")) ; keysignatures
                    (lambda () (disp "stem")))) ; stem directives /voice presets
                (MapToSelection ActionChooserExample)!# 
