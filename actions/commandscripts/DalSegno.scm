;;;DalSegno
(if (d-Directive-chord? "DalSegno")
	(d-DirectiveDelete-chord  "DalSegno")
	(begin 
	(d-DirectivePut-chord-postfix "DalSegno" "-\\markup {  \\italic \"D.S. \"  \\tiny \\raise #1  \\musicglyph #\"scripts.segno\"}")
	(d-DirectivePut-chord-display "DalSegno" "D.S.")))
	(d-SetSaved #f)
;;;End of scheme script