;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;InsertStandaloneDirective
(let ()
(define current (d-DirectiveGet-standalone-postfix (d-DirectiveGetTag-standalone)))
(let script ((answer (d-GetUserInput "Insert Lilypond" "Give Lilypond text to insert" (if current current ""))))
	(if (and answer (not (string=? answer "")))
		(begin
			(if current 
				(d-DirectiveDelete-standalone (d-DirectiveGetTag-standalone)))
			(StandAloneDirectiveProto (cons (d-GetChecksum answer) answer)))
		#f)))