;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;;;;;; ForAllLayouts
 (let ((tag (d-DirectiveGetTag-standalone)) )
  (if tag
	(begin
		(d-DirectivePut-standalone-display tag "")
		(d-DirectivePut-standalone-x tag 0)
		(d-DirectivePut-standalone-y tag 0))))
(d-SetSaved #f)
(d-RefreshDisplay)
