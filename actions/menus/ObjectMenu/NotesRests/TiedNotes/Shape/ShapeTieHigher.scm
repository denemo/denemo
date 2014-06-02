;;ShapeTieHigher
(let ((tag "TieShape")(amount "0")(data #f)(height #f))
        (set! data (d-DirectiveGet-standalone-data tag))
        (if data
            (begin
                (set! data (eval-string data)))
             (begin
                (set! data '((0 . 0) (0 . 0) (0 . 0) (0 . 0)))))
       
        (set! amount (d-GetUserInput (_ "Height of Tie") (_ "Give amount to raise (unit = staff line space):") amount))
        (if (and amount (string->number amount))
            (begin
                (set! amount (string->number amount))
                ;modify y offsets by the amount
                (list-set! data 0 (cons (car (list-ref data 0)) (+ amount (cdr (list-ref data 0)))))
                (list-set! data 1 (cons (car (list-ref data 1)) (+ amount (cdr (list-ref data 1)))))
                (list-set! data 2 (cons (car (list-ref data 2)) (+ amount (cdr (list-ref data 2)))))
                (list-set! data 3 (cons (car (list-ref data 3)) (+ amount (cdr (list-ref data 3)))))
                            
                (d-Directive-standalone tag)
                (d-DirectivePut-standalone-minpixels tag 30) 
                (d-DirectivePut-standalone-display tag "~")
                (d-DirectivePut-standalone-data tag (format #f "'~A" data))
                (d-DirectivePut-standalone-postfix tag (string-append "\\shape #'" (format #f "~A" data) " Tie "))))
        (d-RefreshDisplay)
        (d-SetSaved #f))
