;;;LyricVerseDynamic
;(let ((choice (d-GetUserInput (_ "Lyric Dynamic") (_ "Give dynamic to insert between syllables:") "f")))

(let ((choice (RadioBoxMenu "ff" "f" "p" "pp")))
  (if choice
    (begin
    	(d-InsertTextInVerse (string-append "\\set stanza = \\markup { \" \" \\dynamic \"" choice "\" \" \"}\n"))
        (d-SetSaved #f))))
