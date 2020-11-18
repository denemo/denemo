;ExportAsMusicXML
(define-once ExportAsMusicXML::warning #t)
(if  ExportAsMusicXML::warning
	(begin
		(set!  ExportAsMusicXML::warning #f)
		(d-WarningDialog "Your score will be exported to MusicXML music interchange format. Note this does not preserve all the information - you must save your score as well")))
(d-ExportMusicXML)
	