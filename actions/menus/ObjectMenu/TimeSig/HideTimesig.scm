;;;HideTimesig
;(let ((lilycontext (d-GetOption  (string-append "Score" stop "Staff" stop "Voice" stop))))
(let ((lilycontext "Staff"))
(if lilycontext
    (begin 
        (if (d-Directive-timesig? "HideTimesig")
            (d-DirectiveDelete-timesig "HideTimesig")
            (begin
                (d-DirectivePut-timesig-prefix "HideTimesig"  (string-append  "\\once \\override " lilycontext ".TimeSignature #'stencil = ##f "  ))
                (d-DirectivePut-timesig-gy "HideTimesig" 60)
                (d-DirectivePut-timesig-graphic "HideTimesig" "\n⋃\nDenemo\n24")))
            (d-SetSaved #f))))
            
