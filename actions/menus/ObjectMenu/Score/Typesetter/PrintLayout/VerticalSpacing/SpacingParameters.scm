;;;;;;;;SpacingParameters
(let ((tag #f)(choice
		(d-PopupMenu (list (cons (_  "Last title to start of movement") 'markup-system-spacing)
				   (cons (_ "End of movement to next title") 'score-markup-spacing)
				   (cons (_ "End of movement to next (untitled) movement") 'score-system-spacing)
				   (cons (_ "System to system") 'system-system-spacing)
				   (cons (_ "Title to title") 'markup-markup-spacing)
				   (cons (_ "Movement to bottom margin") 'last-bottom-spacing)
				   (cons (_ "Top margin to (untitled) movement") 'top-system-spacing)
				   (cons (_ "Top margin to first title") 'top-markup-spacing))))
		(type 
			(d-PopupMenu (list (cons (_  "Basic Distance") 'basic-distance)
								(cons (_  "Minimum Distance") 'minimum-distance)
								(cons (_  "Padding") 'padding)
								(cons (_  "Stretchability") 'stretchability)))))
								
    (DenemoSpacingParams (symbol->string choice) (symbol->string choice) type))