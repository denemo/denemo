;;;PrintTwoReduced
(d-PushPosition)
(let ((saved (d-GetSaved)))
	(ForAllMovements "(d-GoToPosition #f 1 1 1) (d-SmallerStaff) (d-VoiceOne)
			  (d-GoToPosition #f 2 1 1) (d-SmallerStaff) (d-VoiceTwo)  (d-SetCurrentStaffAsVoice)")
	
	(d-TypesetForScript "(d-PrintTwoReduced)")
	
	(ForAllMovements  "(d-GoToPosition #f 1 1 1)  (d-SmallerStaff) (d-VoiceAuto) (if (StemDirective?) (d-DeleteObject))	
	  (d-GoToPosition #f 2 1 1) (d-SmallerStaff) (d-VoiceAuto) (if (StemDirective?) (d-DeleteObject))  (d-SetCurrentVoiceAsStaff)")
	(d-SetSaved saved))
(d-PopPosition)
