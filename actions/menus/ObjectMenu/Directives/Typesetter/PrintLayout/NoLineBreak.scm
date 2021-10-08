 ;;;;;;;;NoLineBreak
 (let ((tag "NoLineBreak")(params NoLineBreak::params))
 (if (d-Directive-standalone?  tag)
    (d-InfoDialog "This sign denotes No Line Break. The typesetter will keep this measure with the previous one.  Use the Conditional Directives menu to make it applicable only to specific layouts. Delete using Del or Backspace key.")
    (begin
        (d-DirectivePut-standalone tag)
        (d-DirectivePut-standalone-postfix tag "\\noBreak")
        (d-DirectivePut-standalone-gy tag -40)
            (d-DirectivePut-standalone-graphic tag "
LX
Denemo
36")
        (d-DirectivePut-standalone-minpixels tag 10)
        (d-SetSaved #f)
        (d-RefreshDisplay)
        (if (not params) (SetDirectiveConditional "standalone" tag)))))
