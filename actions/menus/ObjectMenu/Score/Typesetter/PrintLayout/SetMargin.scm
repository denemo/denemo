;;;SetMargin
(let ((tag #f) (value #f)(two-sided (d-Directive-paper? "TwoSidedMargins")))
    (define (set-value tag margin value)
        (d-DirectivePut-paper-data tag value)
        (d-DirectivePut-paper-postfix tag (string-append margin value "\\mm\n")))
        

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
                    ((inner) (begin
                                (set! tag "InnerMargin")
                                (set! margin "inner-margin = ")
                                (set! value (d-DirectiveGet-paper-data tag))))
                    ((outer) (begin
                                (set! tag "OuterMargin")
                                (set! margin "outer-margin = ")
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
                (set! value "20"))
            (case choice
                    ((left) (set! value (DenemoGetUserNumberAsString (_ "Left Margin") "Give Left Margin (mm)" value)))
                    ((inner) (set! value (DenemoGetUserNumberAsString (_ "Inner Margin") "Give Inner Margin (mm)" value)))
                    ((outer) (set! value (DenemoGetUserNumberAsString (_ "Outer Margin") "Give Outer Margin (mm)" value)))
                    ((right) (set! value (DenemoGetUserNumberAsString (_ "Right Margin") "Give Right Margin (mm)" value)))
                    ((top) (set! value (DenemoGetUserNumberAsString (_ "Top Margin") "Give Top Margin (mm)" value)))
                    ((bottom) (set! value (DenemoGetUserNumberAsString (_ "Bottom Margin") "Give Bottom Margin (mm)" value))))
            (if value
                (set-value tag margin value)))
                
    (define choice (RadioBoxMenu
          (if two-sided (cons (_ "Inner Margin") 'inner) 
                        (cons (_ "Left Margin")  'left))
          (if two-sided (cons (_ "Outer Margin") 'outer) 
                        (cons (_ "Right Margin") 'right)) 
          (cons (_ "Top Margin") 'top)
          (cons (_ "Bottom Margin") 'bottom)))
          
    (do-choice choice)                          
    (d-SetSaved #f))
