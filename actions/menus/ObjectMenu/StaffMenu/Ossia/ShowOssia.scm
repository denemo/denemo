;;; ShowOssia
(let ((tag "ShowOssia"))
  (if (d-Directive-standalone? tag)
        (d-InfoDialog (_ "This marks the start of a passage to be shown on the ossia staff"))
        (begin 
            (d-Directive-standalone tag)
            (d-DirectivePut-standalone-postfix tag "\\startStaff \\unHideNotes")
            (d-DirectivePut-standalone-display tag (_ "Show"))
            (d-DirectivePut-standalone-ty tag 40)
            (d-DirectivePut-standalone-minpixels tag 30)
            (d-MoveCursorRight)
            (d-RefreshDisplay))))
