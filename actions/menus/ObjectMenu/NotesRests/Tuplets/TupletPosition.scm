;;;TupletPosition
(let ((tag "TupletPosition")(choice (RadioBoxMenu (cons (_ "Up") (cons "UP " "▲")) (cons (_ "Down") (cons "DOWN " "▼")) (cons (_ "Auto") (cons "'tuplet-number::calc-direction "   "▼▲")))))
  (if choice
    (begin
        (d-DirectivePut-standalone tag)
        (d-DirectivePut-standalone-graphic tag (string-append "\n3" (cdr choice) "\nSerif\n20\n" DENEMO_WEIGHT_BOLD "\n" DENEMO_SLANT_ITALIC))
        (d-DirectivePut-standalone-minpixels tag 30)
        (d-DirectivePut-standalone-postfix tag (string-append "\\once \\override TupletBracket #'direction = #" (car choice))))
    (d-InfoDialog (_ "Cancelled")))
   (d-SetSaved #f)
  (d-RefreshDisplay))