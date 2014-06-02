;;ShapeTieWider
(let ((tag "TieShape")(amount 0.0)(data #f)(width #f))
        (set! data (d-DirectiveGet-standalone-data tag))
        (if data
            (begin
                (set! data (eval-string data))
                (set! width (* 2 (car (list-ref data 3)))))
             (begin
                (set! data '((0 . 0) (0 . 0) (0 . 0) (0 . 0)))))

        (if (not width)
            (set! width 0.0))
        (set! amount (number->string width))
        (set! amount (d-GetUserInput (_ "Width of Tie") (_ "Give additional width (unit = staff line space):") amount))
        (if (and amount (string->number amount))
            (begin
                (set! amount (exact->inexact (/ (string->number amount) 2)))
                ;modify the (car (list-ref data 3)) to become amount ...
                (list-set! data 3 (cons amount (cdr (list-ref data 3))))
                (list-set! data 0 (cons (- amount) (cdr (list-ref data 0))))
                (d-Directive-standalone tag)
                (d-DirectivePut-standalone-minpixels tag 30) 
                (d-DirectivePut-standalone-display tag "~")
                (d-DirectivePut-standalone-data tag (format #f "'~A" data))
                (d-DirectivePut-standalone-postfix tag (string-append "\\shape #'" (format #f "~A" data) " Tie "))))
        (d-RefreshDisplay)
        (d-SetSaved #f))
