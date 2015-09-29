;;;FirstPageNumber
(let ((tag "FirstPageNumber"))
(define (FirstPageNumber::set value)
    (d-DirectivePut-paper-postfix tag (string-append "
first-page-number = " value "\n")))
(if (equal? FirstPageNumber::params "edit")
    (begin
        (set! FirstPageNumber::params #f)
        (d-DirectiveDelete-paper tag)))
(if FirstPageNumber::params
    (begin
        (FirstPageNumber::set (number->string FirstPageNumber::params)))
    (if (d-Directive-paper? tag)
        (begin
            (d-DirectiveDelete-paper tag)
            (d-InfoDialog (_ "Default First Page Number")))
        (let ((value
        (d-GetUserInput (_ "First Page Number") (_ "Give number for first page") "2")))
        (if (and (string? value) (string->number value))
         (FirstPageNumber::set  value)))))        
(d-SetSaved #f)) 
