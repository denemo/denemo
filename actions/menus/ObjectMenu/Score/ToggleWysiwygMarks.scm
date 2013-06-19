;;;;;;;; ToggleWysiwygMarks
(let ((tag "ToggleWysiwygMarks"))
      (d-SetSaved #f)
      (if (d-Directive-score? tag)
      	(d-DirectiveDelete-score tag)
      	(begin
      		(d-LilyPondInclude "red-dot.ly")
     		(d-DirectivePut-score-prefix tag "\\layout {
    \\context {
      \\Score
      \\printRefpoint ##f #'all-grobs
    }
  }
  "
     		)
     		 (d-DirectivePut-score-display tag (_ "Wysiwyg Marks On"))
     		 (d-InfoDialog (_ "Typeset score will have graphical objects marked with red dots\nUse these for accurate tweaking of positions via wysiwyg operations on the typeset score with mouse")))))
   