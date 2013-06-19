;;;SharpenInitialKeysigs
		(d-PushPosition)
		(while (MoveUpStaffOrVoice))
		(d-IncrementInitialKeysig)
		(while (MoveDownStaffOrVoice)
			(d-IncrementInitialKeysig))
		(d-PopPosition)
        