;;;;CreateButtonForObject
(let ((script "")(tag (d-DirectiveGetForTag-standalone))(palette (d-SelectPalette #f)))
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
                
        
  (define (clone-directive tag theargs) (disp "tag is " tag " and args " (car theargs) " and " (cdr theargs) "\n\n")
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
        (if (> n 1)
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
                            (disp "Doing note height " n "\n\n")
                            (clone-note-directives n)
                            (if (d-SwapNotesAtCursorHeight #f)
                                (begin
                                    (d-SwapNotesAtCursorHeight)
                                    (set! script (string-append script "(d-SwapNotesAtCursorHeight)"))))
                            (loop (1+ n)))))))))
  (set! script (string-append script "(d-RefreshDisplay)"))
  (let ((label (d-GetUserInput (_ "Object Clone") (_ "Give (unique) label for button: ") (_ "mylabel"))))
  
    (if label
            (let ((tooltip (d-GetUserInput (_ "Object Clone") (_ "Give tooltip for button: ") (_ "Inserts object"))))
                (if (not tooltip)
                    (set! tooltip "No tooltip"))
                    (d-CreatePaletteButton palette  label tooltip script)))))
    