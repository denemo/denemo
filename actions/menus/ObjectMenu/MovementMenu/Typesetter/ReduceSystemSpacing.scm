;;;ReduceSystemSpacing
(let ((tag "ReduceSystemSpacing" ))
(if (d-Directive-layout? tag)
    (d-InfoDialog (_ "Normal System Spacing is restored") #f))
(ToggleDirective "layout" "postfix" tag "
  between-system-padding = #0.1
  between-system-space = #0.1
  ragged-last-bottom = ##f
  ragged-bottom = ##f
"))

