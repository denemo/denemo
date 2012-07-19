;;;;;;;;PageBreak
(if (d-Directive-standalone?  "PageBreak")
	(d-InfoDialog "This sign denotes a Page Break. The music after this point will start on new page. This page break must be on a bar line. Use the Conditional Directives menu to make the page break applicable only to specific layouts. Delete using Del or Backspace key.")
	(begin
	(d-DirectivePut-standalone "PageBreak")
	(d-DirectivePut-standalone-postfix "PageBreak" "\\pageBreak")
	(d-DirectivePut-standalone-minpixels "PageBreak" 50)
		(d-DirectivePut-standalone-gy "PageBreak" -40)
	(d-DirectivePut-standalone-graphic "PageBreak" "
⁋
Denemo
36")
	(d-DirectivePut-standalone-minpixels "PageBreak" 10)))
(d-SetSaved #f)
(d-RefreshDisplay)
