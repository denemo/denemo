;;;FlattenInitialKeysigs
		(d-PushPosition)
		(while (MoveUpStaffOrVoice))
		(d-IncrementInitialKeysig -1)
		(while (MoveDownStaffOrVoice)
			(d-IncrementInitialKeysig -1))
		(d-PopPosition)
        