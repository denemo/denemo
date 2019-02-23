;;ChangeStencil
(let ((tag "ChangeStencil")
        (choice (RadioBoxMenuPairs  
        	(cons (_ "Augmentation Dot") "Dots") 
        	(cons (_ "Accidental") "Accidental") 
        	(cons (_ "Note Head") "NoteHead")
        	(cons (_ "Clef") "Staff.Clef")
        	(cons (_ "Clef Modifier") "Staff.ClefModifier")
        	(cons (_ "Cue Clef") "Staff.CueClef")
        	(cons (_ "Cue End Clef") "Staff.CueEndClef")
        	(cons (_ "Key Signature") "Staff.KeySignature")
        	(cons (_ "Key Cancellation") "Staff.KeyCancellation")
        	(cons (_ "Time Signature") "Staff.TimeSignature")
        	(cons (_ "Arpeggio") "Staff.Arpeggio")
        	(cons (_ "Breath") "Staff.BreathingSign")
        	(cons (_ "Custos") "Staff.Custos")
        	(cons (_ "Fingering") "Fingering")
        	(cons (_ "Multi-Measure Rest") "MultiMeasureRest")
        	(cons (_ "Rest") "Rest")
            )))
   (if choice
   	(let ((markup (d-GetUserInputWithSnippets (string-append (_ "Markup to use for ") (car choice))))) 
		   (if markup
		   	(begin
		   		(d-SetSaved #f)
		   		(StandAloneDirectiveProto (cons tag (string-append  "\\once \\override " 
				                                        (cdr choice) ".stencil = #(lambda (grob) (grob-interpret-markup grob #{\\markup {"
				                                        (cdr markup) "} #}))\n"))
				            #f #f (car choice) 20))))))