;;;AttachedText
(let ((text #f) (tag "AttachedText") (params AttachedText::params) (markup #f)(current #f)
            (position #f) (shift ""))
     (set! current (d-DirectiveGet-chord-data tag))
     (if (not current)
        (set! current (d-DirectiveGet-chord-display tag)))
     (if (and params (not (equal? "edit" params)))
        (begin
            (if (string? params)
                (set! text (cons params (string-append "\"" params "\"")))
                (let ((choice (caar params)))  ;;; params is a list of pairs
                     ;(set! text (cons current (string-append "\"" current "\""))) 
                        (case choice
                        ((padding)   (set! shift (string-append "-\\tweak #'padding #"  (cdar params) "  ")))
                        ((direction)  (set! position (cdar params)))            
                        ((offsetx)  (set! position (string-append "-\\tweak #'X-offset #'" (cdar params) " -\\tweak #'Y-offset #'"  (cdadr params) " -"))))))))
      (if (not position)
         (begin   
            (set! position (RadioBoxMenuList (list (cons (_ "Above") "^")  (cons (_ "Below") "_") (cons (_ "Auto Position") "-"))))))
            
     (if (not current)
                (set! current "")) 
    (if (not text)
        (set! text (d-GetUserInputWithSnippets (_ "Text") (_ "Give text to appear with note/chord:\nThe characters \\, \", §, { and } have a special meaning in the text,\nthe backslash \\ starts some LilyPond syntax, the others must be paired.\nTo apply italic or bold to a group of words enclose them in {}, e.g. \\bold {These words are bold}.\nOther markup commands \\super, \\tiny etc, see LilyPond documentation.") current)));;cannot popup menu after this, it runs gtk_main
 
    (if text
        (if (string-null? (car text))
  
            (begin
                (let ((confirm (d-GetUserInput (d-DirectiveGet-chord-display tag) (_ "Delete this text?") (_ "y"))))
                 (if (equal? confirm (_ "y"))
                    (begin
                        (d-DirectiveDelete-chord tag)
                        (d-SetSaved #f))
                    (d-InfoDialog (_ "Cancelled")))))
            (begin 
                (if position
                   (begin
                            (set! markup (cdr text))
                            (set! text (car text))
                            (d-DirectivePut-chord-display tag (string-pad-right text 5))
                            (d-DirectivePut-chord-data tag text)
                            (d-DirectivePut-chord-postfix tag (string-append shift position "\\markup\\scale #'(1 . 1)\\column{" markup "}"))
                            (d-DirectivePut-chord-minpixels tag 20)
                            (d-SetSaved #f)))))))
            
