;;HideBarlines
    (if (d-Directive-score? "HideBarlines")
    	(d-DirectiveDelete-score "HideBarlines")
    	(begin
     	 (d-SetSaved #f)
     	 (d-DirectivePut-score-postfix "HideBarlines" "\\override Score.BarLine #'stencil = ##f")
     	 (d-DirectivePut-score-display "HideBarlines" (_ "Hide Barlines"))))
  