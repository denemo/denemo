 ;;; BookMarkRNamed
;;; Named RehearsalMark searchable as bookmark.
;;; by Nils Gey. RTS Modified to avoid poluting the global namespace with the variable user-input
(let ((user-input "XXX"))
	(set! user-input (d-GetUserInput "Named Bookmark" "Give a name" "X"))
	(if user-input   ;in case the user pressed Escape do nothing	
 	(begin	
	  (d-Directive-standalone "RehearsalMark")
	  (d-DirectivePut-standalone-display "RehearsalMark" user-input)
	  (d-DirectivePut-standalone-ty  "RehearsalMark" -40)
	  (d-DirectivePut-standalone-tx  "RehearsalMark" 10)
	  (d-DirectivePut-standalone-postfix "RehearsalMark"  (string-append  " \\once \\override Score.RehearsalMark #'self-alignment-X = #left"  "\r \\mark \\markup {\\bold " user-input "}" ) )
	  (d-DirectivePut-standalone-gx  "RehearsalMark"  15)
	  (d-DirectivePut-standalone-gy  "RehearsalMark" -40)
	  (d-DirectivePut-standalone-minpixels  "RehearsalMark"  10)
	  (d-DirectivePut-standalone-graphic "RehearsalMark" "RehearsalMark")
	  ))
	(d-RefreshDisplay))
