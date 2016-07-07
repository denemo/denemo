;;;;CreateButtonForObject
(let ((script "")(tag (d-DirectiveGetForTag-standalone))(palette #f))
    (define (list-io direction type)
        (list 
                (string-append "d-Directive" direction "-" type "-prefix")
                (string-append "d-Directive" direction "-" type "-postfix")
                (string-append "d-Directive" direction "-" type "-display")
                (string-append "d-Directive" direction "-" type "-tx")
                (string-append "d-Directive" direction "-" type "-ty")
                (string-append "d-Directive" direction "-" type "-graphic")
                (string-append "d-Directive" direction "-" type "-gx")
                (string-append "d-Directive" direction "-" type "-gy")
                (string-append "d-Directive" direction "-" type "-grob")
                (string-append "d-Directive" direction "-" type "-midibytes")
                (string-append "d-Directive" direction "-" type "-override")
                (string-append "d-Directive" direction "-" type "-minpixels")))
  (define (create-args type)
    (cons (list-io "Put" type) (list-io "Get" type)))
                
        
  (define (clone-directive tag theargs)
    (let loop ((n (- (length (car theargs)) 1)))
        (define put (list-ref (car theargs) n))
        (define get (eval-string (list-ref (cdr theargs) n)))
        (define val #f)
        (set! val  (get tag))
        (if val
            (begin
                (if (number? val)
                    (begin
                        (set! val (number->string val))
                        (set! script (string-append script "(" put " \"" tag "\" " val ")\n")))
                    (set! script (string-append script "(" put " \"" tag "\" \"" (scheme-escape val) "\")\n")))))
        (if (> n 0)
            (loop (- n 1)))))
            
  (define (clone-note-directives m)
    (set! script (string-append script "(d-CursorToNthNoteHeight " (number->string m) ")\n"))
     (let loop ((n 0))
        (define tag (d-DirectiveGetNthTag-note n))
        (if tag
            (begin
                (clone-directive tag (create-args "note"))
                (loop (1+ n))))))        

 (if tag ;;;standalone directive
    (begin
        (set! script (string-append "(d-Directive-standalone \"" tag "\")\n"))
        (clone-directive tag (create-args "standalone")))
    (begin
        (if (Music?)
            (begin
                (set! script (string-append "(d-InsertChord \"" (d-GetNotes) "\")(d-MoveCursorLeft)\n"))
                ;;;clone chord directives
                (let loop ((n 0))
                    (define tag (d-DirectiveGetNthTag-chord n))
                    (if tag
                        (begin
                            (clone-directive tag (create-args "chord"))
                            (loop (1+ n)))))
                ;;;clone note directives
                 (let loop ((n 1))
                    (if (d-CursorToNthNoteHeight n)
                        (begin
                           
                            (clone-note-directives n)
                            (if (d-SwapNotesAtCursorHeight #f)
                                (begin
                                    (d-SwapNotesAtCursorHeight)
                                    (set! script (string-append script "(d-SwapNotesAtCursorHeight)"))))
                            (loop (1+ n))))))
            (if (Keysignature?)
                (begin
                    (set! script (string-append "(d-InsertKey \"" (d-GetPrevailingKeysigName) "\")(d-MoveCursorLeft)\n"))
                    (let loop ((n 0))
                    (define tag (d-DirectiveGetNthTag-keysig n))
                    (if tag
                        (begin
                            (clone-directive tag (create-args "keysig"))
                            (loop (1+ n)))))
                )
                (if (Timesignature?)
                    (begin
                        (set! script (string-append "(d-InsertTimeSig \"" (d-GetPrevailingTimesig) "\")(d-MoveCursorLeft)\n"))
                        (let loop ((n 0))
                        (define tag (d-DirectiveGetNthTag-timesig n))
                        (if tag
                            (begin
                                (clone-directive tag (create-args "timesig"))
                                (loop (1+ n)))))
                    
                    )
                    (if (Clef?)
                        (begin
                            (set! script (string-append "(d-InsertClef \"" (d-GetPrevailingClef) "\")(d-MoveCursorLeft)\n"))
                            (let loop ((n 0))
                            (define tag (d-DirectiveGetNthTag-clef n))
                            (if tag
                                (begin
                                    (clone-directive tag (create-args "clef"))
                                    (loop (1+ n)))))
                        
                        )
                        (if (TupletOpen?)
                            (begin
                                (set! script (string-append "(d-StartTriplet)(d-MoveCursorLeft)(d-SetTuplet \"" (d-GetTuplet) "\")\n"))
                                (let loop ((n 0))
                                (define tag (d-DirectiveGetNthTag-clef n))
                                (if tag
                                    (begin
                                        (clone-directive tag (create-args "tuplet"))
                                        (loop (1+ n))))))
                            (if (TupletClose?)
                                (begin
                                    (set! script "(d-EndTuplet)(d-MoveCursorLeft)")
                                    (let loop ((n 0))
                                    (define tag (d-DirectiveGetNthTag-clef n))
                                    (if tag
                                        (begin
                                            (clone-directive tag (create-args "tuplet"))
                                            (loop (1+ n))))))))))))))
                     
  (if (or tag (Music?) (Keysignature?)(Timesignature?)(Clef?)(TupletMarker?))
    (begin
    (set! palette (d-SelectPalette #f))
    (set! script (string-append script "(d-RefreshDisplay)(d-MoveCursorRight)"))
    (let ((label (d-GetUserInput (_ "Object Clone") (_ "Give (unique) label for button: ") (_ "mylabel"))))
        (if label
                (let ((tooltip (d-GetUserInput (_ "Object Clone") (_ "Give tooltip for button: ") (_ "Inserts object"))))
                    (if (not tooltip)
                        (set! tooltip "No tooltip"))
                (if (not (d-CreatePaletteButton palette label tooltip script))
                    (d-WarningDialog (_ "Failed - duplicate label, no palette?")))))))
    (d-WarningDialog (_ "Not implemented yet"))))
    
