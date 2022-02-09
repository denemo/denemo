;;FootnoteSeparator
(let ((tag "Footnote Separator")(separator (d-GetUserInputWithSnippets (_ "Give footnote separator or Cancel for none") "" "\\fill-line{\\line {---------------}}"))
)
;footnote-separator-markup = ##f 
(d-DirectivePut-score-prefix tag (string-append "\\paper { footnote-separator-markup = "
	(if separator (string-append "\\markup {" (car separator) "}") "#f") " } "))
(d-DirectivePut-score-display tag "footnote separator")
(d-SetSaved #f))
	
	