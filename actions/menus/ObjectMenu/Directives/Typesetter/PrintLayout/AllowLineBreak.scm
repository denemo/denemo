;;AllowLineBreak
 (let ((tag "AllowLineBreak")(params AllowLineBreak::params))
 (if (d-Directive-standalone?  tag)
    (d-InfoDialog (_ "This sign allows a Line Break. The bar can be broken across a line at this point. Delete using Del or Backspace key."))
    (begin
        (if (d-IsVoice)
            (begin
                (d-WarningDialog (_ "The cursor is on a voice. Put line breaks in the main staff that the voice belongs to.")))
            (let ((choice #f))
                (d-DirectivePut-standalone tag)
                (d-DirectivePut-standalone-postfix tag "\\bar \"\"")
                (d-DirectivePut-standalone-graphic tag "\n|\nDenemo\n24")
                (d-DirectivePut-standalone-minpixels tag 10)    
                (d-RefreshDisplay)  
                (if (not params)
                    (SetDirectiveConditional "standalone" tag))
                (d-SetSaved #f)
                )))))
