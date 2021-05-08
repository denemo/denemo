;;;SingleExtender
(let ((fig (d-GetUserInput (_ "Figured Bass Extenders") (_ "Give Figure to Extend") "0")))
	(if (string? fig)
		(d-EditFiguredBass (string-append "figures $\\bassFigureExtendersOn <" (scheme-escape fig) ">4
\\bassFigureExtendersOff$"))))


