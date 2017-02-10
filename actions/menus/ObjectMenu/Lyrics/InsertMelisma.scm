;InsertMelisma
(let ((choice (RadioBoxMenu
	(cons (_ "During Word, with Slur") " -- ")
	(cons (_ "Word End, with Slur") " __ ")
	(cons (_ "During Word, no Slur") " -- _ ")
	(cons (_ "Word End, no Slur") " __ _ ")
	(cons (_ "Extend Melisma") " _ "))))
  (if choice
     (begin
         (d-InsertTextInVerse choice)
         (d-SetSaved #f))))