;;; Part of NotationMagick 
				(let ((return (NotationMagick::AskNewNotationstring)))
					(if return
						 (NotationMagick::InsertMemberRandomlyLy return)
						#f))
		