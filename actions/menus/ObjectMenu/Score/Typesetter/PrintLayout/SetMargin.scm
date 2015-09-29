;;;SetMargin
(let ((tag #f) (value #f))
    (define (set-value tag margin value)
        (d-DirectivePut-paper-data tag value)
        (d-DirectivePut-paper-postfix tag (string-append margin value "\\cm\n")))
        

    (define (do-choice choice)
            (define margin #f)
            (define tag #f)
            (case choice
                    ((left) (begin
                                (set! tag "LeftMargin")
                                (set! margin "left-margin = ") 
                                (set! value (d-DirectiveGet-paper-data tag))))
                    ((right) (begin
                                (set! tag "RightMargin")
                                (set! margin "right-margin = ")
                                (set! value (d-DirectiveGet-paper-data tag))))
                    ((top) (begin
                                (set! tag "TopMargin")
                                (set! margin "top-margin = ")
                                (set! value (d-DirectiveGet-paper-data tag)))) 
                    ((bottom) (begin
                                (set! tag "BottomMargin")
                                (set! margin "bottom-margin = ")
                                (set! value (d-DirectiveGet-paper-data tag)))))
            (if (not value)
                (set! value "2"))
            (case choice
                    ((left) (set! value (d-GetUserInput (_ "Left Margin") "Give Left Margin" value)))
                    ((right) (set! value (d-GetUserInput (_ "Right Margin") "Give Right Margin" value)))
                    ((top) (set! value (d-GetUserInput (_ "Top Margin") "Give Top Margin" value)))
                    ((bottom) (set! value (d-GetUserInput (_ "Bottom Margin") "Give Bottom Margin" value))))
            (if value
                (set-value tag margin value)))
                
    (define choice (RadioBoxMenu
          (cons (_ "Left Margin")   'left)   
          (cons (_ "Right Margin")   'right)   
          (cons (_ "Top Margin") 'top)
          (cons (_ "Bottom Margin") 'bottom)))
          
    (do-choice choice)                          
    (d-SetSaved #f))