;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;Slur Down:
(d-DirectivePut-standalone-display "SlurDirection" "SlurDown")
(d-MoveCursorLeft)
(d-DirectivePut-standalone-postfix "SlurDirection" " \\slurDown" )
(d-DirectivePut-standalone-minpixels "SlurDirection" 60)
(d-MoveCursorRight)
(d-RefreshDisplay) 