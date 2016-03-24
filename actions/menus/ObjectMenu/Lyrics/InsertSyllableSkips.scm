;InsertSyllableSkips
(let ((count #f)(end #f))
  (d-SynchronizeLyricCursor)
  (d-SetPoint)
  (set! end (d-SyllableCount))
  (d-GoToMark)  
  (set! count (- end (d-SyllableCount)))
  (if (<= count 0)
      (set! count (d-GetUserInput (_ "Skip Syllables") 
                                  (_ "Give number of syllables to skip") 
                                  "10"))
   (set! count (number->string count)))
    (if count 
            (begin
                (d-InsertTextInVerse (string-append "\n\\repeat unfold "  (+1 count) " \\skip 1\n"))
                (d-SetSaved #f))))
