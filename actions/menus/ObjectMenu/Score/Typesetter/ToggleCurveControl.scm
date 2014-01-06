;;;;;;;; ToggleCurveControl
(let ((tag "ToggleCurveControl"))
      (d-SetSaved #f)
      (if (d-Directive-score? tag)
      	(d-DirectiveDelete-score tag)
      	(begin
      		(d-LilyPondInclude "control-points.ly")
     		(d-DirectivePut-score-prefix tag "\\layout {
      					 \\override Slur #'stencil = #(display-control-points #t)
      					 \\override Tie #'stencil = #(display-control-points #t)
       					\\override PhrasingSlur #'stencil = #(display-control-points #t)
  		}
  "
     		)
     		 (d-DirectivePut-score-display tag (_ "Curve Control Points On"))
     		 (d-InfoDialog (_ "Typeset score will have curves marked with control points\nUse these for accurate reshaping of curves via wysiwyg operations on the typeset score with mouse")))))
   