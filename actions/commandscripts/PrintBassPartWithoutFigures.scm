;;;PrintBassWithoutFigures
(let ((saved (d-GetSaved)) (response 1))
 (d-GoToPosition #f response 1 1)
 (let loop ()
 	(if  (d-GoToPosition #f response 1 1)
		(if (not (d-HasFigures))
			(begin 
				(set! response (+ 1 response))
				(loop)))))
  (set! response (number->string response))
  (if (d-HasFigures)
	(let ((command  (string-append "(d-GoToPosition #f " response " 1 1) (d-HideFiguredBass)")))
		(ForAllMovements command)
		(d-PrintPart)
		(set! command  (string-append "(d-GoToPosition #f " response " 1 1) (d-ShowFiguredBass)"))
		(ForAllMovements command)
		(d-SetSaved saved))
	(d-InfoDialog "No staff has figured bass, or figured bass is hidden for printing already - use Print Part")))

