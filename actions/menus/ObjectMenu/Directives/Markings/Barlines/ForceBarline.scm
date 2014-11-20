;;;ForceBarline 
(define-once ForceBarline::warned  (_ "Warning: Do not use this command to create an upbeat (or pickup) bar.\nIt does not affect which notes appear in which measures.\nIt simply draws a barline at the point in question, if no other barline will be drawn.\nUse the Measures->Anacrusis (Upbeat) command for a short measure."))
(disp "have " ForceBarline::params "\n\n")

(let ((tag "ForceBarline"))
   (if (or (d-Directive-standalone? tag) (equal? ForceBarline::params "edit"))
      (d-InfoDialog (_ "This object instructs the LilyPond typesetter to draw a barline here.\nThe timing is not altered, use a hidden time signature change or the short measure or anacrusis command to do that."))
     (begin
         (if ForceBarline::warned
            (begin
                (d-WarningDialog ForceBarline::warned)
                (set! ForceBarline::warned #f)))
        (d-DirectivePut-standalone-minpixels tag 20)
        (d-MoveCursorLeft)
        (d-DirectivePut-standalone-postfix tag " \\bar \"|\"")
        (d-DirectivePut-standalone-gx  tag  13)
        (d-DirectivePut-standalone-gy  tag  0)
        (d-DirectivePut-standalone-graphic tag "Barline")
        (d-MoveCursorRight)
        (d-RefreshDisplay)
        (d-SetSaved #f))))
