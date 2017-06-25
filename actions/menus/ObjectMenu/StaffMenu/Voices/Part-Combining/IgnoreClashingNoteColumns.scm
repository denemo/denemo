;;;IgnoreClashingNoteColumns
(let ((tag "IgnoreClashingNoteColumns"))
	(if (d-Directive-score? tag)
		(begin
			(d-DirectiveDelete-score tag)
			(d-InfoDialog (_ "Clashing Note Columns will generate a warning from the LilyPond typesetter")))
		(begin
			;(d-DirectivePut-score-prefix tag "\\layout {\\override NoteColumn #'ignore-collision = ##t }\n")
			(d-DirectivePut-score-prefix tag "\n#(ly:expect-warning \"ignoring too many clashing note columns\")\n")
			(d-DirectivePut-score-display tag (_ "Ignore Clashing Columns"))
			(d-DirectivePut-score-override tag 4)))
	(d-SetSaved #f))
