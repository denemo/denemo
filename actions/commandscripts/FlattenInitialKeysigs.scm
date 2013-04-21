;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;FlattenInitialKeysigs
		(d-PushPosition)
		(while (MoveUpStaffOrVoice))
		(d-IncrementInitialKeysig -1)
		(while (MoveDownStaffOrVoice)
			(d-IncrementInitialKeysig -1))
		(d-PopPosition)
        