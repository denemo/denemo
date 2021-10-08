;;;;;;PageBreak
(let ((tag "PageBreak"))
(if (d-Directive-standalone?  tag)
    (d-InfoDialog "This sign denotes a Page Break. The music after this point will start on new page. This page break must be on a bar line. Use the Conditional Directives menu to make the page break applicable only to specific layouts. Delete using Del or Backspace key.")
    (begin
        (if (d-IsVoice)
            (begin
                (d-WarningDialog "The cursor is on a voice. Put line breaks in the main staff that the voice belongs to."))
            (begin
                (if (and (not (Appending?)) (not (d-Directive-standalone?)))
                    (d-WarningDialog (_ "This Page Break will have no effect if there is no barline at this point.  You can use Allow Line/Page Break to insert an invisible one if you need it")))
                (d-DirectivePut-standalone tag)
                (d-DirectivePut-standalone-postfix tag "\\pageBreak")
                (d-DirectivePut-standalone-minpixels tag 50)
                (d-DirectivePut-standalone-gy tag -25)
                (d-DirectivePut-standalone-graphic tag "
⁋
Denemo
26")
                (d-DirectivePut-standalone-minpixels tag 10)
                (d-RefreshDisplay)
                (SetDirectiveConditional "standalone" tag)
                (d-SetSaved #f))))))
