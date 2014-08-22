;LyricFontSize
(let ((text #f))
      (set! text (d-GetUserInput (_ "Font Size") 
                                                (_ "Give font size magnification (0 = default, negative = smaller)") 
                                                "-4"))
       (if (and text (string->number text))
            (begin
            	(d-InsertTextInVerse (string-append "\n\\override LyricText.font-size = #"  text "\n")))
        	(d-SetSaved #f)))
