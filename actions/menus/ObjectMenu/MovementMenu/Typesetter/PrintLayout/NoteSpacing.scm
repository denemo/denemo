;NoteSpacing
(let* ((tag "NoteSpacing") (layout (d-GetLayoutName))(id (d-GetLayoutId))
		(restrict (RadioBoxMenu  (cons (string-append (_ "Restrict to layout: " ) layout) layout) (cons (_ "For all Layouts") 'all))))
	(if restrict
		(begin
			(if (eq? restrict 'all)
				(set! restrict #f))
			(if restrict
				(set! tag (string-append tag "\n" restrict)))
			(if (d-Directive-layout? tag)
				(begin
					(d-DirectiveDelete-layout tag)
					(d-WarningDialog (string-append (if restrict layout "")
							(if restrict (_ " This movement will have default horizontal note spacing for this layout")
										 (_ " This movement will have default horizontal note spacing")))))
				(begin
					(d-DirectivePut-layout-postfix tag (string-append " \\override Score.SpacingSpanner.common-shortest-duration = #(ly:make-moment "
						(DenemoGetDuration (_ "Choose basis for horizontal spacing ( 𝅝    is tightest, 𝅘𝅥𝅲    is loosest)"))
						") "))
					(if restrict
						(d-DirectivePut-layout-allow tag id)))))
		(d-WarningDialog (_ "Cancelled"))))
