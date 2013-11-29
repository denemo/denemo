;;;SelectMidiImportStrategy
(let ((choice (d-GetOption  (string-append (_ "Guided Import") stop  (_ "Automatic Import") stop))))
   (cond
     ((boolean? choice)
      (d-InfoDialog (_ Cancelled)))
     ((equal? choice (_ "Guided Import") )
      (d-GuidedMidiImport))
     ((equal? choice (_ "Automatic Import"))
		(d-ImportMidi))))
