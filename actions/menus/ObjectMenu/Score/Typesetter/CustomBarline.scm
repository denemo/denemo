;;;CustomBarline
(let* ((tag "CustomBarline")(params CustomBarline::params) (defines (d-DirectiveGet-score-data tag)) (name "A")(start "|")(end "|")(mid "|")(span "|"))
    (define (createTag name)
        (string-append "Bar" name))
    (define (setBarline) ;;; sets the LilyPond to define all the defines listed in Directive-score-data
        
        (d-SetSaved #f)
        (d-DirectivePut-score-display tag (_ "Custom Barlines"))
        (d-DirectivePut-score-override tag (logior DENEMO_OVERRIDE_AFFIX))
        (let ((prefix ""))
            (define (do-append def)
                (set! name (list-ref def 0))
                
                (set! start (list-ref def 1))
                (set! mid (list-ref def 2))
                (set! end (list-ref def 3))
                (set! span (list-ref def 4)) 
                (if (not (string-null? name))
                    (let ((deftag (createTag name)))
                        (d-DirectiveDelete-score (string-append "Allow\n" deftag)) ;; remove old definition
                        (set! name (string-append "-" name))
                        (d-LilyPondDefinition (cons deftag (string-append "\\bar \"" (string-append mid name) "\"\n")))))
                (set! prefix (string-append prefix 
                "\n\\defineBarLine \"" mid name "\" #'(\"" end "\" \"" start "\" \"" span "\")\n")))
            (for-each do-append (eval-string (d-DirectiveGet-score-data tag)))
            (d-DirectivePut-score-prefix tag prefix)))
    (define (remove name)
        (set! defines (delete #f (map (lambda (el) (if (not (equal? name (list-ref el 0))) el #f)) defines))))
     
     
    (define (update-data def) ;; if def is not in defines: adds def to the Directive-score-data and updates LilyPond
        (remove (list-ref def 0))
        (set! defines (cons def defines))
        (d-DirectivePut-score-data tag (string-append "'"  (format #f "~s" defines)))
        (setBarline))

    (define (get-data)
            (set! name (d-GetUserInput (_ "Custom Barline") (_ "Give name (blank to re-define a standard barline)") name))
            (set! mid (d-GetUserInput (_ "Custom Barline") (_ "Give barline char(s) - | : . [ ] ; etc, for the normal appearance of the barline") mid))
            (set! start (d-GetUserInput (_ "Custom Barline") (_ "Give char(s) for line start appearance") start))
            (set! end (d-GetUserInput (_ "Custom Barline") (_ "Give char(s) for line end appearance") end))
            (set! span (d-GetUserInput (_ "Custom Barline") (_ "Give char(s) for appearance when spanning staffs") span))
            (if (and name start mid end span)
                    (list name start mid end span)
                    #f))

 ;;;;;start of procedure
    (if defines
        (set! defines (eval-string defines))
        (set! defines '()))
    (let ((def #f))
        (if params
            (set! def (eval-string params))
            (set! def (get-data)))
        (if def
            (update-data def)
            (d-WarningDialog (_ "Custom Bar Lines unchanged"))))) 
     
   
