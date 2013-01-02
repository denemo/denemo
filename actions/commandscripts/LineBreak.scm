;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
 ;;;;;;;;LineBreak
 (let ((tag "LineBreak"))
 (if (d-Directive-standalone?  tag)
	(d-InfoDialog "This sign denotes a Line Break. The music after this point will start on new line. This line break must be on a bar line. Use the Conditional Directives menu to make the line break applicable only to specific layouts. Delete using Del or Backspace key.")
	(begin
(d-DirectivePut-standalone tag)
(d-DirectivePut-standalone-postfix tag "\\break")
(d-DirectivePut-standalone-gy tag -40)
	(d-DirectivePut-standalone-graphic tag "
L
Denemo
36")
(d-DirectivePut-standalone-minpixels tag 10)
(d-SetSaved #f)
(d-RefreshDisplay))))
