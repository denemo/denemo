;;;PrintPartWithTitlePage
(if  (and (d-Directive-scoreheader? "BookTitle") (d-Directive-staff? "InstrumentName"))
  (let ((saved (d-GetSaved))
	(datefield (d-DirectiveGet-scoreheader-display "BookDate"))
	(instrument (d-DirectiveGet-staff-display "InstrumentName")))
    (d-BookDate instrument)
    (d-PrintPart)
    (if datefield
      (d-BookDate datefield)
      (d-DirectiveDelete-scoreheader "BookDate"))
  (d-SetSaved saved))
  (begin
    (d-WarningDialog (_ "No Book Title, see Score->Titles->Book Titles"))))
  