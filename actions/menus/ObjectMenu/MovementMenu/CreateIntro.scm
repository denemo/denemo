;;;;;;;;;;CreateIntro
(let ( (position (GetPosition)) (timesig (d-InitialTimeSig "query=timesigname")) (numerator #f)(denominator #f))
  (define (createIntroStaff)
    (d-NewStructuredStaff 'initial)
    (d-InitialClef "Treble")
    (d-StaffHidden #t)
    (d-StaffProperties "midi_channel=9")
     (d-DirectivePut-clef-graphic "DrumClef" "DrumClef")
    (d-DirectivePut-clef-override "DrumClef" DENEMO_OVERRIDE_GRAPHIC)
    (d-NonPrintingStaff 'set)
    (d-StaffProperties "denemo_name=Intro"))
  
  (define (writeIntroBar numerator denominator)
                ;;;write an intro bar
    (let loop ((count (string->number numerator)))
      (if (positive? count)
      (begin
        (eval-string (string-append "(d-" (number->string (duration::lilypond->denemo (string->number denominator))) ")"))
        (loop (- count 1)))))
     (d-MuteStaff "unmute"))
  
  (define (deleteToEnd)
    (d-SetMark)
    (d-GoToEnd)
    (d-Cut))
  
  (define firstmeasure #t)
  (define measurenum (list-ref position 2))
  
  (while (d-MoveToStaffUp))
     
  (if (equal? "Intro" (d-StaffProperties "query=denemo_name"))
    (begin
              (set! firstmeasure #f);;we will not need to add an initial intro measure, as there will be one already
              (if (not (None?))
              (if  (equal? (_ "y") (d-GetUserInput (_ "Non Empty Intro staff") (_ "Remove the previous transcription from this measure on?") (_ "y")))
                   (deleteToEnd)
                   (set! firstmeasure 'abort))))
    (begin
        (createIntroStaff)))
      
  (if (not (eq? firstmeasure 'abort))
      (begin    
        (if firstmeasure
            (begin  
              (d-GoToBeginning)
              (set! measurenum (+ 1 measurenum))
              (d-InsertMeasure)
              (set! numerator (car (string-split   timesig #\/)))
              (set! denominator (cadr (string-split  timesig #\/)))
              (writeIntroBar numerator denominator)))      
        (while (d-MoveToStaffDown)
            (if (EmptyMeasure?)
                (begin
                    (d-DirectivePut-standalone-graphic "Blank" "\nBlank\nDenemo\n20")
                    (d-SetDurationInTicks (* 1536 (GetPrevailingTimeSig #t))))))
        (d-GoToPosition #f #f  measurenum (list-ref position 3)))))
      
