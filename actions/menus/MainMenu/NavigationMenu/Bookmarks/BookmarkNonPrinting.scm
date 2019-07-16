;;;RehearsalMarkNonPrinting 
;;;Non Printing Bookmark - note it uses the RehearsalMark tag which allows it to be found via book mark navigation, but should have its own edit script FIXME
(let* ((tag "RehearsalMark")
      (current (d-DirectiveGet-standalone-display tag)))
  (set! current (d-GetUserInput (_ "Insert Non-printing Rehearsal Mark") (_ "Give a name ") (if current current "X") #f))
	(if current 
    (begin
      (d-Directive-standalone tag)
      (d-DirectivePut-standalone-minpixels tag 30)	
      (d-DirectivePut-standalone-override tag DENEMO_OVERRIDE_EDITOR)
      (d-DirectivePut-standalone-display tag current)
      (d-SetSaved #f)
      (d-RefreshDisplay))))
