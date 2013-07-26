;;TextInsideSlur
(let ((tag "TextInsideSlur"))

(d-DirectivePut-standalone tag)
(d-DirectivePut-standalone-postfix tag "\\once \\override TextScript #'avoid-slur = #'inside
  \\once \\override TextScript #'outside-staff-priority = ##f ")
(d-DirectivePut-standalone-display tag "Inside Slur ")
(d-DirectivePut-standalone-minpixels tag 30)
(d-SetSaved #f)
(d-RefreshDisplay))
