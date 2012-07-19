;;;;;;;;;;;;;;; SetBreve
(if (and (defined? 'Snippet::Breve) (> Snippet::Breve 0))
    (d-SelectSnippet Snippet::Breve)
(begin
(d-PushPosition)
(d-InsertMeasureBefore)
(d-Insert0)
	(d-DirectivePut-chord-graphic "Duration" "

emmentaler")
	(d-DirectivePut-chord-override "Duration" (logior DENEMO_OVERRIDE_GRAPHIC DENEMO_ALT_OVERRIDE))
	(d-DirectivePut-chord-prefix "Duration" "\\breve ")		
	(d-SetDurationInTicks (* 2 1536))
(d-CreateSnippetFromObject "Breve")
(d-DeleteMeasure)
(d-PopPosition)))
