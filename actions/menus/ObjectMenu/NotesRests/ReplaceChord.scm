;;;;ReplaceChord
(let ((tag "ReplaceChord")
  (lily #f))
  (set! lily (d-DirectiveGet-chord-prefix tag))
  (if  lily
    (begin
      (if (> (string-length lily) 2)
	(set! lily (string-drop-right lily 2))))
    (set! lily "r"))
  (set! lily (d-GetUserInput "Replace Note/Rest" "Give LilyPond syntax to replace the note or rest at the cursor or blank to delete" lily))
  (if (and lily (> (string-length lily) 0))
    (begin
      (d-DirectivePut-chord-prefix tag (string-append lily "%{"))
      (d-DirectivePut-chord-postfix tag " %}\n")
      (d-DirectivePut-chord-override tag  DENEMO_OVERRIDE_AFFIX)
      (d-DirectivePut-chord-display tag "customized"))
    (d-DirectiveDelete-chord tag)))
(d-SetSaved #f)