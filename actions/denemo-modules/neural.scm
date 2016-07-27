(define (DenemoCreateTrainingData)
    (define first #f)
    (define number_of_patterns 0)
    (define output "")
    (define num_durations 10)  ;;tuplet dotted rest next-rest demisemiquaver semiquaver quaver crotchet minim  semibreve
    (define midi (list (cons 0 0) (cons 0 0) (cons 0 0)))
    (define MidiNoteStarts (make-vector 256 #f))
    (define num-midi 0)
    (define num-notes 0)
    (define (note-or-rest?) (or (Note?) (Rest?)))
    (define (string-from midi)
        (define start (car (car midi)))
        (define str "")
        (let loop ((data midi))
            (define on (car (car data)))
            (define off (cdr (car data)))
            (disp "on " on " off " off "\n")
            (set! str (string-append str (number->string (- on start)) " " (number->string (+ (- off start))) " "))
            (if (not (null? (cdr data)))
                (loop (cdr data))))
        str)
 
    (define (next-note) ;;removes the first note from midi and puts the next one on the end
        (define found #f)
        (set! midi (cdr midi)) 
        (let loop ()
            (define note (d-GetRecordedMidiNote))
            (if note
                (let ((tick (d-GetRecordedMidiOnTick))) ;(disp "tick is " tick "\n")
                      
                    (if (not first)
                        (set! first tick))                  
                          
                    (if (< tick 0)
                        (let ((on (vector-ref MidiNoteStarts note)))
                          (if on
                              (begin 
                                 (set! found #t)
                                 (set! num-midi (1+ num-midi))
                                 (set! midi (append! midi (list (cons on (- (- tick) first)))))
                                 (vector-set! MidiNoteStarts note #f))
                              (d-WarningDialog "An off when not on"))) ;;; end of note off
                        (vector-set! MidiNoteStarts note (- tick first))) ;;; end of note on
                    (if (not found)
                                (loop)))))
        (if (not found)
            (let ((last (car (last-pair midi)))) 
                (set! found (positive? (car last)))
                (set! midi (append! midi (list  (cons (car (car midi)) (car (car midi)))))) ; put a pair which will become 0 . 0 when start is subtracted
                (disp "Not found with last " last " and " midi "\n")
                )
            
            found))
    (define (positioning-ok?)
        (let ((num-midi 0)(num-notes 1)(message #f)(seen-rest #f))
            
            (if (not (FirstInSelection Note?))
                (SelectAllInStaff))
                
            (FirstInSelection Note?)
            (d-PushPosition)
            (let loop ()
                (if (and (not message) (NextInSelection note-or-rest?))
                    (if (Rest?)
                        (begin
                            (if seen-rest
                                (set! message (_ "Two successive rests detected. Train the patterns on either side separately"))
                                (begin
                                    (set! seen-rest #t)
                                    (loop)))) ;;;;;;; a check should be done that the rest has the duration of this or the following note.
                        (begin  
                            (if (d-IsTied)
                                (set! message (_ "Cannot train on tied notes")))
                            (set! seen-rest #f)    
                            (set! num-notes (1+ num-notes))
                            (loop)))))
           (if (not message)
                (begin
                    (d-RewindRecordedMidi)
                    (while (d-GetRecordedMidiOnTick)
                        (set! num-midi (1+ num-midi)))
                    (set! num-midi (/ num-midi 2))
                    (if (not (= num-midi num-notes))
                        (set! message (string-append (_ "Different numbers of Notes and MIDI notes: ") (number->string num-notes) " =? " (number->string num-midi))))
                    (if (< num-midi 2)
                        (set! message (_ "At least three notes needed to train")))))
           (if message (d-WarningDialog message))
           (d-PopPosition)
           (not message)))
            
            
            
    (define (rest-status)
        (define this (d-GetNoteBaseDuration))
        (define status " 0 0")
        (d-PushPosition)
        (NextInSelection note-or-rest?)
        (if (Rest?)
            (let ((rest-duration (d-GetNoteBaseDuration)))
                (if (= this rest-duration)
                    (set! status " 1 0")
                    (begin
                        (NextInSelection Note?)
                        (if (= rest-duration (d-GetNoteBaseDuration))
                            (set! status " 0 1"))))))
        (d-PopPosition)
            status)
            
            
;;;;;; the training creating of the training data starts here.
    (if (and (positioning-ok?)(d-RewindRecordedMidi))
        (let ((index #f)(note (d-GetRecordedMidiNote))) ;(disp "note is " note "\n")
              (d-PushPosition)
              (while (and (not (Note?)) (NextInSelection Note?)))
              ;;; we look ahead one notes, so the classifier gets to see one note before and after the note being classified
               (next-note)
              ;;; now compute the string output by appending two lines for each note or note+rest in the pattern, one line is the pattern of outut weights the other the midi timings 
              (let loop ()
                (define this "\n")
                (set! num-notes (1+ num-notes))
                ;this will be the index counting from the end of the weights:
                ;;tuplet dotted rest next-rest demisemiquaver semiquaver quaver crotchet minim  semibreve
                (if (next-note)   ;;;; this sets up midi to hold the input weights, that is, the timings for the note+rest being considered
                    (begin
                        (set! this (string-append this  (string-from midi) "\n"))
                          ;;ignore tuplets
                        (set! this (string-append this "0"))
                        
                        ;;
                        (set! this (string-append this " " (number->string (d-GetDots))))   
                        ;;
                        (set! this (string-append this (rest-status))) ;; " 0 0" if no rest or " 1 0", " 0 1" for rest of this duration/rest of next duration.
                                
                        (set! index (d-GetNoteBaseDuration))      ;;allow 0 1 2  3 4 5
   
                        (set! number_of_patterns (1+ number_of_patterns))
                        (let loop2 ((count 5)) ;;; 5 for demi and semis
                            (set! this (string-append this (if (= count index) " 1" " 0")))
                            (if (positive? count)
                                (loop2 (1- count))))
                        (set! output (string-append output this))
                        (if (NextInSelection Note?)
                                (loop)))))
            (d-PopPosition)
            (if (= num-midi num-notes)
                (d-Train number_of_patterns 6 10 output)
                (d-WarningDialog (_ "Number of notes from cursor position onwards mis-matches number of MIDI notes")))
            (set! output (string-append    (number->string number_of_patterns) " 6 "  (number->string num_durations) "\n" output))
            (disp "************\n" output "\n*****************\nNumber of MIDI notes detected: " num-midi "\nNumber of notes detected: " num-notes"\n"))
        (d-WarningDialog (_ "Make a recording of some notes, then select them to train the neural network to recognize those notes from that recording"))))
              
(define (DenemoConvert)
    (define dots-threshold 0.25) ;threshold for the dotted note weight to trigger a dot
    (define first #f)
    (define number_of_patterns 0)
    (define num_durations 10) ;;tuplet dotted rest next-rest demisemiquaver semiquaver quaver crotchet minim  semibreve
    (define midi (list (cons 0 0) (cons 0 0) (cons 0 0)))
    (define MidiNoteStarts (make-vector 256 #f))
    (define num-midi 0)
    (define note 0)
    (define pending-rest #f)

    (define (string-from midi)
        (define start (car (car midi)))
        (define str "")
        (let loop ((data midi))
            (define on (car (car data)))
            (define off (cdr (car data)))
            ;(disp "on " on " off " off "\n")
            (if (and (zero? on) (zero? off))
                (set! str (string-append str " 0 0 "))
                (set! str (string-append str (number->string (- on start)) " " (number->string (+ (- off start))) " ")))
            (if (not (null? (cdr data)))
                (loop (cdr data))))
        (string-append str "\n"))
        
    (define (inject weights)
        (define weight-list (string-split weights #\space))
        (define heaviest 0)
        (define rest #f)
        (define which 0) (disp "Weights are \n" weights "\n")
        (let loop ((count 4)) ;;; 4 to 9  demisemiquavers up to semibreve
            (define this (string->number (list-ref weight-list count)))
            (if (> this heaviest)
                (begin
                    (set! heaviest this)
                    (set! which count)))
            (set! count (1+ count))
            (if (< count (length weight-list))
                (loop count)))
                
         (set! which (- which 4));;; because skipping over non-duration weights  
         
         (if pending-rest
            (pending-rest))

        (set! rest (string->number (list-ref weight-list 2)))
        (set! pending-rest (string->number (list-ref weight-list 3)))
        (if (or (> rest 0.2) (> pending-rest 0.2))
                (if (< rest pending-rest)
                    (begin
                        (set! rest (lambda () #f))
                        (set! pending-rest (case which
                                                ((5) d-InsertWholeRest)
                                                ((4) d-InsertHalfRest)
                                                ((3) d-InsertQuarterRest)
                                                ((2) d-InsertEighthRest)
                                                ((1) d-InsertSixteenthRest)
                                                ((0) d-InsertThirtySecondRest)
                                                ((-4) d-EnterRest))))
                    (begin
                        (set! rest d-EnterRest)
                        (set! pending-rest #f)))
                (begin
                    (set! pending-rest #f)
                    (set! rest (lambda () #f))))
                
                
         (case which
            ((0)  (d-Insert5)(rest))
            ((1)  (d-Insert4)(rest))
            ((2)  (d-Insert3)(rest))
            ((3)  (d-Insert2)(rest))
            ((4)  (d-Insert1)(rest))
            ((5)  (d-Insert0)(rest))
            ((-4) (d-Insert2)(rest))) ;;; all duration weights zero, use crotchet
            
            (if (< heaviest 0.3)
                (d-SetNonprinting))
        (if (> (string->number (list-ref weight-list 1)) dots-threshold)
            (d-AddDot)))
            
            
    
    
    
    (define (next-note) ;;removes the first note from midi and puts the next one on the end
        (define found #f)
        (set! midi (cdr midi)) 
        (let loop ()
            (set! note (d-GetRecordedMidiNote))
            (if note
                (let ((tick (d-GetRecordedMidiOnTick))) ;(disp "tick is " tick "\n")
                      
                    (if (not first)
                        (set! first tick))                  
                          
                    (if (< tick 0)
                        (let ((on (vector-ref MidiNoteStarts note)))
                          (if on
                              (begin 
                                 (set! found #t)
                                 (set! num-midi (1+ num-midi))
                                 (set! midi (append! midi (list (cons on (- (- tick) first)))))
                                 (vector-set! MidiNoteStarts note #f))
                              (disp "with " note " and " tick "An off when not on"))) ;;; end of note off
                        (vector-set! MidiNoteStarts note (- tick first))) ;;; end of note on
                    (if (not found)
                                (loop)))))
        (if (not found)
            (set! midi (append! midi (list (cons 0 0))))))

    (if (d-RewindRecordedMidi)
        (let ((index #f))

               (d-PushPosition)
              
              ;;; we look ahead one notes, so the classifier gets to see one notes before and after the note being classified
               (next-note)

              (let loop ()
                (define this-note (d-GetNoteForMidiKey note))
                (next-note)
                (disp "Classifying " (string-from midi) "\n")
                (inject (d-Classify (string-from midi)))
                (d-PutNoteName this-note)
                (set! number_of_patterns (1+ number_of_patterns))
                ;(disp "Midi is " midi " and " this-note "\n")
                (if (positive?  (car (caddr midi)))
                        (loop)))
            (d-PopPosition)
            (disp "*************************\nNumber of MIDI notes detected: " num-midi"\n"))
         (d-WarningDialog (_ "No MIDI Recording"))))

