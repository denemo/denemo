;;;OrnamentFlat
(let ((tag "OrnamentFlat")  (params OrnamentFlat::params) (direction "-") (markup "\\tweak outside-staff-priority #50 -\\markup { \\tiny \\flat }" ))
	(if (list?  params)
		(let ((offsetx #f) (offsety #f)(padding #f))
			(cond
				 ((eq? (car (list-ref params 0)) 'offsetx)
						(set! offsetx (cdr (list-ref params 0)))
						(set! offsety (cdr (list-ref params 1)))
						 (set! markup 
				   			(string-append "-\\tweak #'X-offset #" offsetx "  -\\tweak #'Y-offset #" offsety "  -"  markup)))
				 ( (eq? (car (list-ref params 0)) 'direction)
				 	(set! markup (string-append (cdr (list-ref params 0)) " " markup)))		 
			   	( (eq? (car (list-ref params 0)) 'padding)
			   		(set! padding (cdr (list-ref params 0)))
			   		 (set! markup (string-append "-\\tweak padding #"  padding " -" markup))))
			(d-DirectivePut-chord-postfix tag markup) ) 
	(ChordAnnotation "OrnamentFlat"  (string-append direction markup) #f LG-Flat)))
(d-SetSaved #f)

	