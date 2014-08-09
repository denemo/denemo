;;;OrnamentFlat
(let* ((tag "OrnamentFlat")  (params OrnamentFlat::params) (data  (d-DirectiveGet-chord-data tag)) (direction "-") 
     (markup #f) (base-markup "\\tweak self-alignment-X #-1.5 \\tweak outside-staff-priority #50 -\\markup { \\tiny \\flat }" ))
    (if data
        (begin
            (set! data (eval-string data))
            (set! direction (list-ref data 0))
            (set! markup (list-ref data 1))))
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
            (ChordAnnotation tag  (string-append direction markup) #f LG-Flat)))
    (if (and (not (eq? params 'finished)) (d-Directive-chord? tag))
        (d-DirectivePut-chord-data tag (format #f "(list ~s ~s)" direction markup))))
(d-SetSaved #f)
