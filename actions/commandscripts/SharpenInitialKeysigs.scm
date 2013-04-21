;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;SharpenInitialKeysigs
		(d-PushPosition)
		(while (MoveUpStaffOrVoice))
		(d-IncrementInitialKeysig)
		(while (MoveDownStaffOrVoice)
			(d-IncrementInitialKeysig))
		(d-PopPosition)
        