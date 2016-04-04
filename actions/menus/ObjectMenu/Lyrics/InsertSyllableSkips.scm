;InsertSyllableSkips
(let ((count #f)(end #f))
  (d-SetPoint)
  (set! end (d-SyllableCount))
  (d-GoToMark)  
  (set! count (+ 1 (- end (d-SyllableCount))))
  (if (<= count 1)
      (begin
      	(set! count (d-GetUserInput (_ "Skip Syllables") 
                                  (_ "Give number of syllables to skip") 
                                  "10"))
          (if count 
          	(set! count (string->number count))))
     (d-SynchronizeLyricCursor -1))      
    (if count 
            (begin
               (set! count (number->string  count))
                (d-InsertTextInVerse (string-append "\n\\repeat unfold " count  " \\skip 1\n"))
                (d-SetSaved #f))))
