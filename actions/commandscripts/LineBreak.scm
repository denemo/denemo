;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
 ;;;;;;;;LineBreak
 (if (d-Directive-standalone?  "LineBreak")
	(d-InfoDialog "This sign denotes a Line Break. The music after this point will start on new line. This line break must be on a bar line. Use the Conditional Directives menu to make the line break applicable only to specific layouts. Delete using Del or Backspace key.")
	(begin
(d-DirectivePut-standalone "LineBreak")
(d-DirectivePut-standalone-postfix "LineBreak" "\\break")
(d-DirectivePut-standalone-gy "LineBreak" -40)
	(d-DirectivePut-standalone-graphic "LineBreak" "
L
Denemo
36")
(d-DirectivePut-standalone-minpixels "LineBreak" 10)
(d-RefreshDisplay)))
