;;MinimumFret
(let ((tag "MinimumFret") (value #f))
  (set! value (d-DirectiveGet-standalone-display tag))
  (if (not value)
    (set! value "4"))
  (set! value (d-GetUserInput (_ "Minimum Fret Number") (_ "Give lowest fret number wanted: ") value))
  (if value
    (begin

      (d-Directive-standalone tag)
      (d-DirectivePut-standalone-postfix tag (string-append "\\set TabVoice.minimumFret = #" value " "))
      (d-DirectivePut-standalone-graphic tag (string-append "\n" (_ "F") "\nDenemo 24"))
      (d-DirectivePut-standalone-display tag value)
      (d-DirectivePut-standalone-tx tag 10)
      (d-DirectivePut-standalone-minpixels tag 30)
      (d-SetSaved #f)
      (d-RefreshDisplay))))
