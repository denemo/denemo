;;;NumericTimeSig
(let ((tag "NumericTimeSig"))
  (if (d-Directive-timesig? tag)
    (d-DirectiveDelete-timesig tag)
    (begin
      (d-DirectivePut-timesig-prefix tag "\\numericTimeSignature\n")
      (d-DirectivePut-timesig-graphic tag "\nN\nDenemo\n12")
      (d-DirectivePut-timesig-gy tag 0)
      (d-DirectivePut-timesig-minpixels tag 30)))
  
(d-SetSaved #f)
(d-RefreshDisplay))
