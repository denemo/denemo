;;;MakeDirectiveConditional
(let ((params MakeDirectiveConditional::params))
   (if (d-Directive-standalone?)
		(d-ChooseCondition)
		(if (Music?) 
			(let ((tag/type (d-ChooseTagAtCursor)))
				(SetDirectiveConditional #f (cons (if (cdr tag/type) "note" "chord")   (car tag/type))))
				(if (Clef?)
					(SetDirectiveConditional #f (cons "clef"   (d-ChooseTagAtCursor )))
					(if (Timesignature?)
							(SetDirectiveConditional #f (cons "timesig"   (d-ChooseTagAtCursor )))
							(if (Keysignature?)
								(SetDirectiveConditional #f (cons "keysig"   (d-ChooseTagAtCursor )))))))))
					
							
