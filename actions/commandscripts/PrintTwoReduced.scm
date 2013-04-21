;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;PrintTwoReduced
(d-PushPosition)
(let ((saved (d-GetSaved)))
	(ForAllMovements "(d-GoToPosition #f 1 1 1) (d-SmallerStaff) (d-InitialVoiceOne)
			  (d-GoToPosition #f 2 1 1) (d-SmallerStaff) (d-InitialVoiceTwo)  (d-SetCurrentStaffAsVoice)")
	
	(d-TypesetForScript "(d-PrintTwoReduced)")
	
	(ForAllMovements  "(d-GoToPosition #f 1 1 1)  (d-SmallerStaff) (d-InitialVoiceAuto) (if (StemDirective?) (d-DeleteObject))	
	  (d-GoToPosition #f 2 1 1) (d-SmallerStaff) (d-InitialVoiceAuto) (if (StemDirective?) (d-DeleteObject))  (d-SetCurrentVoiceAsStaff)")
	(d-SetSaved saved))
(d-PopPosition)