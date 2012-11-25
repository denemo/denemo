;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;ReduceSystemSpacing
(if (or ReduceSystemSpacing::params  (not (d-Directive-layout? "ReduceSystemSpacing")))
	(d-DirectivePut-layout-postfix "ReduceSystemSpacing" "
  between-system-padding = #0.1
  between-system-space = #0.1
  ragged-last-bottom = ##f
  ragged-bottom = ##f
")
	(d-DirectiveDelete-layout  "ReduceSystemSpacing"))
(d-SetSaved #f)
