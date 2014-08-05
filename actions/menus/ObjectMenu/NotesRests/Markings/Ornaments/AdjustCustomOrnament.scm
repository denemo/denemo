;;;EditCustomOrnament
;;;choose a custom ornament at the cursor, change direction/padding
(let ((tag #f))
 (define (do-direction)
        (let ((direction #f)
                (choice #f)
                (menu (list (cons (_ "Up")  "^")  (cons (_ "Down")  "_") (cons (_ "Auto")  "-") )) )
            (set! choice (d-PopupMenu menu))
            (if choice
                (begin
                    (if (defined? (string->symbol (string-append "d-" tag)))
                        (eval-string (string-append "(d-" tag " (list (cons 'direction \"" choice "\")))"))
                        (eval-string (string-append "(d-ToggleCustomOrnament (list \"" tag "\" (cons 'direction \"" choice "\")))")))))))
                                    
                                    
    
  (define (do-padding)
        (let ((padding (d-GetUserInput (_ "Padding") (_ "Give amount of padding required around this item (in staff spaces)") "0.5")))
        (if padding
                (begin
                    (if (defined? (string->symbol (string-append "d-" tag)))
                                (eval-string (string-append "(d-" tag " (list (cons 'padding \"" padding "\")))"))
                                (eval-string (string-append "(d-ToggleCustomOrnament (list \"" tag "\" (cons 'padding \"" padding "\")))")))))))
;;;;;;;; actual procedure
(set! tag (d-DirectiveGetForTag-chord))
(if tag
    (let ((menu "")(choice #f))
        (set! menu (list  (cons (_ "Up/Down") do-direction) (cons (_ "Padding") do-padding) ))
                                    (set! choice (d-PopupMenu menu))
                                    (if choice
                                            (choice)
                                            (disp "cancelled")))
    (d-WarningDialog (_ "No Custom Ornaments at cursor position"))))
