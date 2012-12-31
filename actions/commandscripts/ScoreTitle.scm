;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;ScoreTitle
(d-PushPosition)
(while (d-PreviousMovement))
(SetHeaderField "title" ScoreTitle::params)
(DenemoPrintAllHeaders)
(d-SetSaved #f)
(d-PopPosition)    