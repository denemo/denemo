;;;SetPageCount
(let* ((tag "SetPageCount")(count (d-GetUserInput (_ "Total Page Count") (_ "Give Pages Required\n(0 for optimal): ") "4"))
		(layout (d-GetLayoutName))(id (d-GetLayoutId))
		(restrict (RadioBoxMenu  (cons (string-append (_ "Restrict to layout: " ) layout) layout) (cons (_ "For any Layout") 'all))))
		
	(if restrict
		(begin
			(if (eq? restrict 'all)
				(set! restrict #f))
			(if restrict
				(begin
					(d-DirectivePut-score-minpixels tag 0) ;;ensure any default comes before conditional
					(set! tag (string-append tag "\n" restrict))))
		
			(if (and count (string->number count) (> (string->number count) 0))
				(begin
					(d-DirectivePut-score-prefix tag (string-append "\\paper { page-count=" count "}"))
					(d-DirectivePut-score-display tag (string-append (_ "Page Count") count))
					(if restrict
						(d-DirectivePut-score-allow tag id))
					(d-SetSaved #f))
				(begin
					(d-DirectiveDelete-score tag)
					(d-WarningDialog (_ "Optimal page count restored")))))
		(d-WarningDialog (_ "Cancelled"))))
