;;;;;;;ChooseSeekEditDirectives
(let ((measurenum (d-GetMeasure)))
    (define tags '())
    (define tag #f)
    (define (get-tags)
        (d-CursorToNthNoteHeight 1)
        (let outer-loop ()
            (let loop ((n 0))
                (set! tag (d-DirectiveGetNthTagStrictNote n))
                (if tag
                    (begin
                        (set! tags (assoc-set! tags (string-append  tag " (N)") (cons tag 'note)))
                        (loop (+ 1 n)))))
                        
            (if (d-CursorToNextNoteHeight)
                        (outer-loop)))
                        
        
        (let loop ((n 0))
                (set! tag (d-DirectiveGetNthTag-chord n))
                (if tag
                    (begin
                        (set! tags (assoc-set! tags (string-append  tag " (CRN)") (cons tag 'chord)))
                        (loop (+ 1 n)))))
        
        (if (Timesignature?)                  
            (let loop ((n 0))
                    (set! tag (d-DirectiveGetNthTag-timesig n))
                    (if tag
                        (begin
                            (set! tags (assoc-set! tags (string-append  tag " (T)") (cons tag 'timesigdir)))
                            (loop (+ 1 n))))))
        (if (Keysignature?)        
            (let loop ((n 0))
                    (set! tag (d-DirectiveGetNthTag-keysig n))
                    (if tag
                        (begin
                            (set! tags (assoc-set! tags (string-append  tag " (K)") (cons tag 'keysigdir)))
                            (loop (+ 1 n))))))
        (if (Clef?)                       
            (let loop ((n 0))
                    (set! tag (d-DirectiveGetNthTag-clef n))
                    (if tag
                        (begin
                            (set! tags (assoc-set! tags (string-append  tag " (C)") (cons tag 'clefdir)))
                            (loop (+ 1 n)))))) 
        (if (StemDirective?)                   
            (let loop ((n 0))
                    (set! tag (d-DirectiveGetNthTag-stemdirective n))
                    (if tag
                        (begin
                            (set! tags (assoc-set! tags (string-append  tag " (V)") (cons tag 'voicedir)))
                            (loop (+ 1 n))))))
        (if (d-GetNonprinting)
                   (set! tags (assoc-set! tags (string-append (_ "Non Printing") " (O)") (cons "Non Printing" 'nonprinting))))
        (if (d-IsSlurStart)
                   (set! tags (assoc-set! tags (string-append (_ "Slur Start") " (O)") (cons "Slur Start" 'slurstart))))
        (if (d-IsSlurEnd)
                   (set! tags (assoc-set! tags (string-append (_ "Slur End") " (O)") (cons "Slur End" 'slurend))))
        (if (d-IsTied)
                   (set! tags (assoc-set! tags (string-append (_ "Tied Note") " (CN)") (cons "Tied Note" 'tied))))
        (if (TupletOpen?)              
                 (set! tags (assoc-set! tags (string-append (_ "Tuplet Start") " (O)") (cons "StartTuplet" 'tupletstart))))  
        (if (TupletClose?)              
                 (set! tags (assoc-set! tags (string-append (_ "Tuplet End") " (O)") (cons "EndTuplet" 'tupletend))))
        (if (Keysignature?)              
                 (set! tags (assoc-set! tags (string-append (_ "Key Change") " (O)") (cons "KeySig" 'keysig))))
        (if (Timesignature?)              
                 (set! tags (assoc-set! tags (string-append (_ "Time Signature Change") " (O)") (cons "TimeSig" 'timesig))))
        (if (StemDirective?)              
                 (set! tags (assoc-set! tags (string-append (_ "Stems Direction") " (O)") (cons "StemDir" 'stemdirection))))
        (if (Clef?)              
                 (set! tags (assoc-set! tags (string-append (_ "Clef Change") " (O)") (cons "ClefChange" 'clef))))
                  
        (set! tag (d-DirectiveGetForTag-standalone))
        (if tag       
              (set! tags (assoc-set! tags (string-append  tag " (O)") (cons tag 'standalone))))) 

    ;;;actual procedure
    (d-PushPosition)
    (d-MoveToBeginning)   
    (let loop ((staffnum (d-GetStaff)))
        (get-tags)
        (if (d-NextObject)
            (loop staffnum)
            (if (d-GoToPosition #f (+ 1 staffnum) measurenum 1)
                    (loop (+ 1 staffnum)))))
    (d-PopPosition)
    (d-MoveCursorLeft) ; to include the original position if possible.
    (if (null? tags)
        (d-InfoDialog (_ "Nothing in this movement can be searched for (no ties, slurs, time signature changes, tuplets, directives etc)."))
    (let ((choice (RadioBoxMenuList tags)))
       (if choice
            (d-EditSimilar choice)))))
