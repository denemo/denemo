;;;;TieShape
(let ((tag "TieShape"))
  (define choice (RadioBoxMenu
          (cons (_ "Widen/Narrow")   'widen)   
          (cons (_ "Flatten/Bend More")   'flatten)   
          (cons (_ "Raise/Lower") 'raise)
          (cons (_ "Advanced") 'advanced)))
  (case choice
    ((widen) (d-ShapeTieWider))
    ((raise) (d-ShapeTieHigher))
    ((flatten) (d-ShapeTieFlatter)) 
    ((advanced) (d-DirectiveTextEdit-standalone tag))))