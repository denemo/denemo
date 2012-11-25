;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;; Part of NotationMagick 
				(let ((return (NotationMagick::AskNewNotationstring)))
					(if return
						 (NotationMagick::InsertMemberRandomlyLy return)
						#f))
		