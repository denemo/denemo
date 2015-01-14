;;;LilyPondInclude
(define-once LilyPondInclude::return #f)
(let* ((tag "LilyPondInclude")(params LilyPondInclude::params) (includes (d-DirectiveGet-score-data tag)))
  (define (setinclude) ;;; sets the LilyPond to include all the files listed in Directive-score-data 
    (d-SetSaved #f)
    (d-DirectivePut-score-display tag (_ "Included LilyPond Files"))
    (d-DirectivePut-score-override tag (logior DENEMO_OVERRIDE_AFFIX DENEMO_OVERRIDE_DYNAMIC))
    (let ((prefix ""))
        (define (do-append str)
            (set! prefix (string-append prefix "\\include \"" str "\"\n")))
        (for-each do-append (eval-string (d-DirectiveGet-score-data tag)))
        (d-DirectivePut-score-prefix tag prefix)))
     
     (define (update-data include) ;; if include is not in includes: adds include to the Directive-score-data and updates LilyPond
        (if (not (member include includes))
            (begin
                (set! includes (cons include includes))
                (d-DirectivePut-score-data tag (string-append "'"  (format #f "~s" includes)))
                (setinclude))))
        
    (define (delete-include name) ;; removes name from the Directive-score-data and updates LilyPond
        (if (member name includes)
            (begin
                (set! includes (delete name includes))
                (if (null? includes)
                    (d-DirectiveDelete-score tag)
                    (begin
                        (d-DirectivePut-score-data tag (string-append "'"  (format #f "~s" includes)))
                        (setinclude))))))
    
 ;;;;;start of procedure   
    (set! LilyPondInclude::return #f) ;;; "return" value
    (if includes
        (set! includes (eval-string includes))
        (set! includes '()))

    (cond
        ((equal? params "edit")
            (d-DirectiveTextEdit-score tag))
        ((string? params)
            (update-data params))
        ((equal? params 'refresh)
              (setinclude))
        ((and (pair? params) (eq? (car params) 'query))  
              (set! LilyPondInclude::return (member (cdr params) includes)))
        ((and (pair? params) (eq? (car params) 'delete))
              (delete-include (cdr params)))
         
        (else     
              (let ((include (d-ChooseFile (_ "Include LilyPond File") DENEMO_LILYPOND_DIR (list "*.ily" "*.ly"))))
                    (if include
                        (begin
                            (set! include (d-FilenameFromPath include))
                                    (update-data include))
                        (d-WarningDialog (_ "LilyPond include files unchanged")))))))
