;InsertLyricAlignment
(let ((text  (RadioBoxMenu (cons (_ "Center") "CENTER") (cons (_ "Left") "LEFT")(cons (_ "Right") "RIGHT"))))
       (if text 
            (d-InsertTextInVerse (string-append "\n\\override LyricText.self-alignment-X = #" text  " %{" (_ "Note: start lyrics on a new line") " %}\n"))))
