;;; Part of NotationMagick 
				(let ((return (NotationMagick::AskNewNotationstring)))
					(if return
						 (NotationMagick::InsertListRandomlyLy return)
						#f))
		