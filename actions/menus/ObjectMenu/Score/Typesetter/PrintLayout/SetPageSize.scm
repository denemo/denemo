;;;SetPageSize
(let ((tag "SetPageSize")(width #f)(height #f)(params SetPageSize::params))
    (define data (d-DirectiveGet-paper-data tag))
    (define (set-size data)
        (d-DirectivePut-paper-postfix tag (string-append "
#(set! paper-alist (cons '(\"custom-size\" . (cons (* " (car data) " cm) (* " (cdr data) " cm))) paper-alist))
#(set-paper-size \"custom-size\")"))
        (d-DirectivePut-paper-data tag (format #f "(cons ~s ~s)" width height)))
    (define (do-resize data)
        (set! width (d-GetUserInput (_ "Page Size") (_ "Give page width in cm ") (car data)))
        (if width
            (begin
                (set! height (d-GetUserInput (_ "Page Size") (_ "Give page height in cm ") (cdr data)))
                (if height
                    (set-size (cons width height))))))
    (if data
        (set! data (eval-string data)))
    (if (equal? params "edit")
        (set! params #f))
    (if params
        (set-size (eval-string params))
        (begin
            (if data
                    (begin
                        (let ((choice (RadioBoxMenu (cons (_ "Revert to Default") 'default) (cons (_ "Re-size") 'resize))))
                            (case choice
                                ((default) (d-DirectiveDelete-paper tag))
                                ((resize) 
                                    (do-resize data)))))
                    (do-resize (cons 21.6 27.9))))))
(d-SetSaved #f)
                                    