;;;OrnamentFlat
(disp "called with " OrnamentFlat::params " as param\n\n")
(let ((tag "OrnamentFlat")  (params OrnamentFlat::params)  (markup "^\\markup { \\tiny \\flat }" ))
	(if (list?  params)
		(let ((offsetx #f) (offsety #f)(padding #f))
			(if (eq? (car (list-ref params 0)) 'offsetx)
				(begin
					(set! offsetx (cdr (list-ref params 0)))
					(set! offsety (cdr (list-ref params 1)))
					 (set! markup 
			   		(string-append "-\\tweak #'X-offset #'" offsetx "  -\\tweak #'Y-offset #'" offsety " "  markup)))
			   	(begin
			   		(set! padding (cdr (list-ref params 0)))
			   		 (set! markup 
			   		(string-append "-\\tweak padding # "  padding " " markup))))
			   (d-DirectiveDelete-chord tag)))	  
	(ChordAnnotation "OrnamentFlat"  markup #f LG-Flat))

	