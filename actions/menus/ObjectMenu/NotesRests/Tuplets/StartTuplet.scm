;;StartTuplet
(let ((top #f) (bottom #f) (directiveText #f))
(if StartTuplet::params
  (set! directiveText StartTuplet::params)
  (begin
    (set! top (d-GetUserInput
      (_ "Enter tuplet numerator ")
      (_ "Enter the fraction to multiply the duration by,\nnumerator first. E.g., for triplets, enter 2, then 3. \nNumerator:") "2"))
    (if top
      (begin
        (set! bottom (d-GetUserInput (_ "Enter tuplet denominator") (_ "Enter the fraction's denominator:") "3" ))
        (if bottom
          (set! directiveText (string-append top "/" bottom )))))))

(if directiveText
  (begin
    (if (not (d-GetTuplet))
        (d-StartTriplet))
    (if (not (d-GetTuplet))
      (d-MoveCursorLeft))
    (d-SetTuplet  directiveText)
    (d-SetSaved #f)
    (d-RefreshDisplay))))
