;;;MultiLineTextAnnotation
(let ((text #f) (params  MultiLineTextAnnotation::params) (tag "MultiLineTextAnnotation")(markup #f)(current #f)(scale "0.5")
            (position #f) (shift "") (prefix "<>"))
     (set! current (d-DirectiveGet-standalone-data tag))
     (if current
        (set! prefix (d-DirectiveGet-standalone-prefix tag)))
     
     (if (and params (not (equal? "edit" params)))
        (begin
            (set! position "-")
            (if (string? params)
                (set! text (cons params (string-append "\"" params "\"")))
                (begin;;; this is a list of pairs
                    (if (eq? (car (car params)) 'offsetx)
                        (d-WarningDialog (_ "Sorry, not possible, use Directives->Markings->Textual Annotation instead"))
                        (set! position (cdar params))))))
         (begin   
            (set! position "-")))
            
     (if (not current)
                (set! current ""))
    (if (not text)
        (set! text (d-GetUserInputWithSnippets (_ "Text") (_ "Give text to appear with following note/chord:\nThe characters \\, \", §, { and } have a special meaning in the text,\nthe backslash \\ starts some LilyPond sytax, the others must be paired.\nTo apply italic or bold to a group of words enclose them in {}, e.g. \\bold {These words are bold}.\nOther markup commands \\super, \\tiny etc, see LilyPond documentation.") current)));;cannot popup menu after this, it runs gtk_main
    (if text 
      (begin 
            (if position
               (begin
               		(set! scale (d-GetUserInput (_ "Scaling Text") (_ "Give text size: ") scale));
               		(if (not scale) (set! scale "0.5"))
                        (set! markup (cdr text))
                        (set! text (car text))
                        (if (not (d-Directive-standalone? tag))
                            (d-DirectivePut-standalone tag))
                        (d-DirectivePut-standalone-data tag text)
                        (d-DirectivePut-standalone-display tag text)
                        (d-DirectivePut-standalone-postfix tag (string-append  shift position "\\markup\\scale #'(" scale " . " scale ")\\column{" markup "}"))
                        (d-DirectivePut-standalone-prefix tag prefix)
                        (d-DirectivePut-standalone-minpixels tag 30)
                        (d-RefreshDisplay)
                        (d-SetSaved #f))))
        (begin
            (if (not params)
                (let ((confirm (d-GetUserInput (d-DirectiveGet-standalone-display tag) (_ "Delete this text?") (_ "y"))))
                 (if (and confirm (equal? confirm (_ "y")))
                    (begin
                        (d-DirectiveDelete-standalone tag)
                        (d-SetSaved #f))
                    (d-InfoDialog (_ "Cancelled"))))))))
            
