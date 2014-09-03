 ;;;HideOssia
(let ((tag "HideOssia")) 
    (if (d-Directive-standalone? tag)
        (d-InfoDialog (_ "This marks the end of the passage to be shown on the ossia staff"))
        (begin
            (d-Directive-standalone tag)
            (d-DirectivePut-standalone-postfix tag "\\stopStaff  \\hideNotes ")
            (d-DirectivePut-standalone-display tag (_ "Hide"))
            (d-DirectivePut-standalone-ty tag 40)
            (d-DirectivePut-standalone-minpixels tag 30)
            (d-MoveCursorRight)
            (d-RefreshDisplay))))  
