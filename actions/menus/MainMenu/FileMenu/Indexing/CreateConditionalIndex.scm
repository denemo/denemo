;CreateConditionalIndex
(define (DenemoIndexCommentDisplay comment)
        (d-DirectivePut-score-display "ScoreComment" comment))
        
(let ((condition (d-GetSchemeText)))  
    (if (string-null? condition)
            (d-WarningDialog (_ "No Scheme condition in the Scheme Script window - see View menu"))
            (d-CreateIndex (string-append "(let ()\n" condition ")"))))
       
