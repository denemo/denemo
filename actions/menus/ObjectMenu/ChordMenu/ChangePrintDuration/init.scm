(define (ChangePrintDuration number duration)
	(define stringnumber (number->string number))
	(if (Note?) (begin
		(AttachDirective "note" "prefix"  (cons "Duration" "") (string-append "\\tweak #'duration-log #" stringnumber " "))
		(AttachDirective "note" "graphic" (cons "Duration" "") (string-append "noteheads.s" stringnumber) DENEMO_OVERRIDE_GRAPHIC)	
		;(AttachDirective "note" "midibytes" (cons "Duration" "") DENEMO_OVERRIDE_DURATION) 
	)))
