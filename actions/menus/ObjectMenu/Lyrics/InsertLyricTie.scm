;InsertLyricTie
(let ((choice (RadioBoxMenu
(cons (_ "Printed as \"ˬ\" (lyric tie symbol)") "~")
(cons (_ "Printed as space") "_"))))
  (if choice
     (begin
         (d-InsertTextInVerse choice)
         (d-SetSaved #f))))
