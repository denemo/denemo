;;;BarNumberingInterval
(let* ((tag "BarNumberingInterval")(params BarNumberingInterval::params) (value "5")
	(lilypond-proc  (lambda (pvalue) 
		(d-DirectivePut-score-data tag pvalue)
		(d-DirectivePut-score-postfix tag (string-append "\\override Score.BarNumber.break-visibility = #end-of-line-invisible
                \\set Score.barNumberVisibility = #(every-nth-bar-number-visible " pvalue") ")))))
    (if (number? params)
        (set! value (number->string params)))
    (ManageSystemDirective params 
                           d-Directive-score? 
                           lilypond-proc
                           (lambda () (d-DirectiveGet-score-data  tag))
                           d-DirectiveDelete-score 
                           tag 
                          (_ "Regular Bar Numbering")
                           (_ "Give interval at which to place bar numbers: ") 
                           value 
                           (lambda (value) (and value (string->number value)))
                            (_ "Default bar numbering restored")))         
                
   