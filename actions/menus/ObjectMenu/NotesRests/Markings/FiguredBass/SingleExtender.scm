;;;SingleExtender
;;;Create a single extender for the group of figures on the previous note. Append |0 to the group and use 0 for the extender to leave a little space between the group and the start of the extender
(let ((params SingleExtender::params)(fig #f))
	(set! fig (if params params (d-GetUserInput (_ "Figured Bass Extenders") (_ "Give Figure to Extend") "0")))
	(if (string? fig)
		(d-EditFiguredBass (string-append "figures $\\bassFigureExtendersOn <" (scheme-escape fig) ">4
\\bassFigureExtendersOff$"))))
