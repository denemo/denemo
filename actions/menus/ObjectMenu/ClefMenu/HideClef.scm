;;;HideClef
;(let ((lilycontext (d-GetOption  (string-append "Score" stop "Staff" stop "Voice" stop))))
(let ((lilycontext "Staff"))
(if lilycontext
    (begin 
        (if (d-Directive-clef? "HideClef")
            (d-DirectiveDelete-clef "HideClef")
            (begin
                (d-DirectivePut-clef-prefix "HideClef"  (string-append  "\\once \\override " lilycontext ".Clef #'stencil = ##f"  ))
                (d-DirectivePut-clef-gy "HideClef" 60)
                (d-DirectivePut-clef-graphic "HideClef" "\n⋃\nDenemo\n24")))
            (d-SetSaved #f))))
            
