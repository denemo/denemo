;;;ToggleFingeringVisibilityInScore
(ForAllObjectsInScoreExecute (lambda () (ForAllNotesInChordExecute (lambda () (d-ToggleFingeringVisibility 'noninteractive)))))