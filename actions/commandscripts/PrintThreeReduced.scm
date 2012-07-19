;;;PrintThreeReduced
(d-PushPosition)
(let ((saved (d-GetSaved)))
	(ForAllMovements "(d-GoToPosition #f 1 1 1) (d-SmallerStaff) (d-VoicePreset1)
			  (d-GoToPosition #f 2 1 1) (d-SmallerStaff) (d-VoicePreset2)  (d-SetCurrentStaffAsVoice)
			   (d-GoToPosition #f 3 1 1) (d-SmallerStaff)   (d-SetCurrentStaffAsVoice)
			  ")
	
	(d-TypesetForScript "(d-PrintThreeReduced)")
	
	(ForAllMovements  "(d-GoToPosition #f 1 1 1)  (d-SmallerStaff) (if (StemDirective?) (d-DeleteObject))	
	  (d-GoToPosition #f 2 1 1) (d-SmallerStaff) (if (StemDirective?) (d-DeleteObject))  (d-SetCurrentVoiceAsStaff)
	   (d-GoToPosition #f 3 1 1) (d-SmallerStaff) (if (StemDirective?) (d-DeleteObject))  (d-SetCurrentVoiceAsStaff)
	   ")
	(d-SetSaved saved))
(d-PopPosition)
