;;;;;;;;;;;;;;; SetLonga
(if (and (defined? 'Snippet::Longa) (> Snippet::Longa 0))
    (d-SelectSnippet Snippet::Longa)
(begin
(d-PushPosition)
(d-InsertMeasureBefore)
(d-Insert0)
(d-DirectivePut-chord-graphic "Duration" "

emmentaler")
	(d-DirectivePut-chord-override "Duration" (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_ALT_OVERRIDE))
	(d-DirectivePut-chord-prefix "Duration" "\\longa ")		
(d-SetDurationInTicks (* 4 1536))
(d-CreateSnippetFromObject "Longa")
(d-DeleteMeasure)
(d-PopPosition)))
