;;;;;;;;LineBreak
(let ((tag "LineBreak"))
 (if (d-Directive-standalone?  tag)
    (d-InfoDialog "This sign denotes a Line Break. The music after this point will start on new line. This line break must be on a bar line. Use the Conditional Directives menu to make the line break applicable only to specific layouts. Delete using Del or Backspace key.")
    (begin
        (if (d-IsVoice)
            (begin
                (d-WarningDialog "The cursor is on a voice. Put line breaks in the main staff that the voice belongs to."))

            (let ((choice #f))
                (if (not (d-Directive-standalone?))
                    (GoToMeasureEnd))
                (d-DirectivePut-standalone tag)
                (d-DirectivePut-standalone-postfix tag "\\break")
                (d-DirectivePut-standalone-gy tag -25)
                (d-DirectivePut-standalone-graphic tag "
L
Denemo
26")
                (d-DirectivePut-standalone-minpixels tag 10)    
                (d-RefreshDisplay)
                (if LineBreak::params
                    (d-MoveCursorRight)
                    (SetDirectiveConditional))
                (d-SetSaved #f))))))
