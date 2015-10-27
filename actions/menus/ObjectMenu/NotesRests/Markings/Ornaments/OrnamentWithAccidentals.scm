;;;OrnamentWithAccidentals
(let* ((tag "OrnamentWithAccidentals")  (params OrnamentWithAccidentals::params) (data  (d-DirectiveGet-chord-data tag)) (direction "-") 
    (ornament #f)(above #f)(below #f)
     (markup #f) (base-markup "\\tweak baseline-skip #1 ^\\markup {\\center-column {\\tiny  \\sharp \\musicglyph #\"scripts.turn\" \\tiny \\flat}}" ))
    (define (get-accidental prompt)
        (RadioBoxMenu  (cons prompt   #f) (cons "♯" "sharp") (cons "♭" "flat")   (cons "♮" "natural")  ))
    (define (get-data)
        (set! ornament (RadioBoxMenu 
            (cons (_ "Trill")  "trill")   
            (cons (_ "Turn")  "turn")   
            (cons (_ "Reverse Turn")  "reverseturn")   
            (cons (_ "Mordent")  "mordent")   
            (cons (_ "Prall")  "prall")
            (cons (_ "Up Prall")  "upprall")
            (cons (_ "Down Prall")  "downprall")
            (cons (_ "Custom")  'custom)))
        (if ornament
            (begin
                (set! direction (RadioBoxMenu (cons (_ "Up") "^")    (cons (_ "Down") "_")   (cons (_ "Auto") "-")))
                (set! above (get-accidental (_ "No accidental above")))
                (set! below (get-accidental (_ "No accidental below")))
                (if (eq? ornament 'custom)
                        (let ((data (GetDefinitionDataFromUser)))
                            (if data
                                (let ((epsfile (list-ref (eval-string data) 1))(size (list-ref (eval-string data) 2)))
                                    (set! ornament (list-ref (eval-string data) 0))
                                    (set! markup (string-append "\\tweak self-alignment-X #-0.8 \\tweak TextScript.padding #2.5 \\tweak baseline-skip #1 " direction "\\markup {\\center-column {" 
                                                    (if above (string-append "\\tiny  \\" above) "")
                                                    " \\epsfile #X #" size " #\"" epsfile "\"" 
                                                    (if below (string-append "\\tiny  \\" below) "")  "}}"          )))
                                (begin
                                    (d-WarningDialog (_ "No Definitions have been created for this score"))
                                    #f)))
                        (begin
                            (set! markup (string-append "\\tweak self-alignment-X #-0.8 \\tweak baseline-skip #1 " direction "\\markup {\\center-column {" 
                                                    (if above (string-append "\\tiny  \\" above) "")
                                                    " \\musicglyph #\"scripts." ornament "\""
                                                    (if below (string-append "\\tiny \\" below) "") "}}")))))))
     
    (if data
        (begin
            (set! data (eval-string data))
            (set! direction (list-ref data 0))
            (set! above (list-ref data 1))

            (set! ornament (list-ref data 2))
            (set! below (list-ref data 3))
            (set! base-markup (list-ref data 4))
            (if (not params)
                (let ((choice (RadioBoxMenu (cons (_ "Delete") 'delete) (cons (_ "Edit") 'edit))))
                      (case choice
                            ((delete) (d-DirectiveDelete-chord tag)
                                        (set! params 'finished))
                            ((edit) (set! params "edit"))
                            (else (set! params 'finished)))))))

       
    (if (not markup)
        (set! markup base-markup))
    (if (list?  params)
        (let ((offsetx #f) (offsety #f)(padding #f))
            (cond
             ((eq? (car (list-ref params 0)) 'offsetx)
                    (set! offsetx (cdr (list-ref params 0)))
                    (set! offsety (cdr (list-ref params 1)))
                     (set! markup 
                        (string-append "\\tweak #'X-offset #" offsetx "  -\\tweak #'Y-offset #" offsety "  -"  base-markup)))
             ( (eq? (car (list-ref params 0)) 'direction)
                (set! direction (cdr (list-ref params 0))))      
            ( (eq? (car (list-ref params 0)) 'padding)
                (set! padding (cdr (list-ref params 0)))
                 (set! markup (string-append "\\tweak padding #"  padding " " base-markup))))
            (d-DirectivePut-chord-postfix tag (string-append direction markup)))
        (if (equal? params "edit")
            (begin
            (set! params 'finished)
            (d-AdjustCustomOrnament tag))
            (begin
                (if (and (not (eq? params 'finished)) (get-data))
                    (begin
                        (set! base-markup markup)
                        (ChordAnnotation tag  markup #f #f "~" direction))
                    (set! params 'finished)))))
    (if (and (not (eq? params 'finished)) (d-Directive-chord? tag))
        (d-DirectivePut-chord-data tag (format #f "(list ~s ~s ~s ~s ~s)" direction above ornament below base-markup))))
(d-SetSaved #f)
