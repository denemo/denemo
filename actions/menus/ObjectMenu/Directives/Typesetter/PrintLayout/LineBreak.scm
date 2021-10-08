;;;;;;;;LineBreak
(let ((tag "LineBreak")(params LineBreak::params))
 (if (d-Directive-standalone?  tag)
    (d-InfoDialog "This sign denotes a Line Break. The music after this point will start on new line. This line break must be on a bar line. Use the Conditional Directives menu to make the line break applicable only to specific layouts. Delete using Del or Backspace key.")
    (begin
        (if (d-IsVoice)
            (begin
                (d-WarningDialog "The cursor is on a voice. Put line breaks in the main staff that the voice belongs to."))

            (begin
                (if (and (not params) (not (Appending?)) (not (d-Directive-standalone?)))
                    (d-WarningDialog (_ "This Line Break will have no effect if there is no barline at this point.  You can use Allow Line/Page Break to insert an invisible one if you need it")))
                (d-DirectivePut-standalone tag)
                (d-DirectivePut-standalone-postfix tag "\\break")
                (d-DirectivePut-standalone-gy tag -25)
                (d-DirectivePut-standalone-graphic tag "
L
Denemo
26")
                (d-DirectivePut-standalone-minpixels tag 10)    
                (d-RefreshDisplay)
                (if params
                    (d-MoveCursorRight)
                    (SetDirectiveConditional "standalone" tag))
                (d-SetSaved #f))))))
