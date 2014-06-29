;;;AttachedText
(let ((text #f) (tag "AttachedText")(markup #f)(current #f)
            (position #f) (shift ""))
     (set! current (d-DirectiveGet-chord-display tag))
     (if (and AttachedText::params (not (equal? "edit" AttachedText::params)))
        (begin
            (set! position "-")
            (if (string? AttachedText::params)
                (set! text (cons AttachedText::params (string-append "\"" AttachedText::params "\"")))
                (begin;;; this is a list of pairs
                    (if (eq? (car (car AttachedText::params)) 'offsetx)
                        (d-WarningDialog (_ "Sorry, not possible, use Directives->Markings->Textual Annotation instead"))
                        (set! position (cdar AttachedText::params))))))
         (begin   
            (set! position (d-PopupMenu (list (cons (_ "Above") "^")  (cons (_ "Below") "_") (cons (_ "Auto Position") "-")))) ))
            
     (if (not current)
                (set! current ""))
    (if (not text)
        (set! text (d-GetUserInputWithSnippets (_ "Text") (_ "Give text to appear with note/chord:\nThe characters \\, \", §, { and } have a special meaning in the text,\nthe backslash \\ starts some LilyPond sytax, the others must be paired.\nTo apply italic or bold to a group of words enclose them in {}, e.g. \\bold {These words are bold}.\nOther markup commands \\super, \\tiny etc, see LilyPond documentation.") current)));;cannot popup menu after this, it runs gtk_main
    (if text 
      (begin 
            (if position
               (begin
                        (set! markup (cdr text))
                        (set! text (car text))
                        (d-DirectivePut-chord-display tag  text )
                        (d-DirectivePut-chord-postfix tag  (string-append shift position "\\markup\\scale #'(.5 . .5)\\column{" markup "}"))
                        (d-DirectivePut-chord-minpixels  tag 20)
                        (d-SetSaved #f))))
        (begin
            (if (not AttachedText::params)
                (let ((confirm (d-GetUserInput (d-DirectiveGet-chord-display tag) (_ "Delete this text?") (_ "y"))))
                 (if (and confirm (equal? confirm (_ "y")))
                    (begin
                        (d-DirectiveDelete-chord tag)
                        (d-SetSaved #f))
                    (d-InfoDialog (_ "Cancelled"))))))))
            
