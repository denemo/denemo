(d-MouseInsertion)

(define (InitializeTypesetting)
  (d-PushPosition)
  (d-CheckScore)
  (if (not CheckScore::return)
    (let ((ok #f))
      (d-InfoDialog (_ "Score Check: Error in this measure"))
      (set! ok (RadioBoxMenu (cons (_ "Print Anyway") "y") (cons (_ "Cancel") "n")))
            (if (equal? ok "n")
                (begin
                    (d-PopPushPosition);;goes to the position on the top of the stack, replacing the top of the stack with the position before it went
                    (d-PopPosition);;goes to the error position that was pushed
                    (exit)))))
    (d-PopPosition))
(d-SetSaved #t)
(display "Simple Profile\n")
