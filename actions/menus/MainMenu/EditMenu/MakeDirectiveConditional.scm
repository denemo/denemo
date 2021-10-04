;MakeDirectiveConditional
(let ((params MakeDirectiveConditional::params))
   (if (d-Directive-standalone?)
		(d-ChooseCondition)
		(if (Music?) (d-ChooseCondition)
				(if (Clef?)
					(SetDirectiveConditional #f (cons "clef"   (d-ChooseTagAtCursor )))
					(if (Timesignature?)
							(SetDirectiveConditional #f (cons "timesig"   (d-ChooseTagAtCursor )))
							(if (Keysignature?)
								(SetDirectiveConditional #f (cons "keysig"   (d-ChooseTagAtCursor )))))))))
					
							
