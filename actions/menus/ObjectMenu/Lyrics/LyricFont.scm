;LyricFont
(let ((text #f))
      (set! text (d-GetUserInput (_ "Font Name") 
                                                (_ "Give font name") 
                                                "Times"))
       (if text 
            (begin
            	(d-InsertTextInVerse (string-append "\n\\override LyricText.font-name = #\""  text "\"\n")))
        	(d-SetSaved #f)))
