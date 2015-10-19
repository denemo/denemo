;;;LimitInterSystemSpace
(let* ((tag "LimitInterSystemSpace") (params LimitInterSystemSpace::params) (value "1.2")
    (lilypond-proc  (lambda (pvalue) (d-DirectivePut-paper-data tag pvalue) (d-DirectivePut-paper-postfix tag (string-append "\npage-limit-inter-system-space = ##t\npage-limit-inter-system-space-factor = " pvalue)))))
    (if (number? params)
        (set! value (number->string params)))
    (ManageSystemDirective params 
                           d-Directive-paper? 
                           lilypond-proc
                           (lambda () (d-DirectiveGet-paper-data  tag))
                           d-DirectiveDelete-paper 
                           tag 
                           (_ "Spacing Between Systems") 
                           (_ "Give spacing limit (1=no extra space)") 
                           value 
                           (lambda (value) (and value (string->number value)))
                           (_ "Customized spacing limit removed")))
    
