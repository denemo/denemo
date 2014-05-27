;;;CrescendoTextSpanner
(if (Music?)
	(let ((tag "CrescendoTextSpanner")(text #f))
	(if (d-Directive-chord? tag)
		(d-DirectiveDelete-chord tag)
		(begin
			(set! text (d-GetUserInput (_ "Crescendo Text Spanner") (_ "Give text ") "poco"))
			(if text
				(begin
					(d-DirectivePut-chord-prefix tag  (string-append
						"\\set crescendoText = \\markup {\\italic { " text "}}\\set crescendoSpanner = #'text "))

					(d-DirectivePut-chord-postfix tag "\\<")
					(d-DirectivePut-chord-override tag DENEMO_OVERRIDE_AFFIX)
					(d-DirectivePut-chord-display tag  text)))))
	(d-SetSaved #f)))
;;;End of scheme script