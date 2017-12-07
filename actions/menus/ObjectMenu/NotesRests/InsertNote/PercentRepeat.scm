;;;PercentRepeat
;;;repeats the selection the given number of times
(if PercentRepeat::params
    (d-InfoDialog (_ "This Denemo Directive is part of a set of four creating a \"Percent Repeat\". Be sure to delete them all if you delete one of them."))
    (if (d-IsInSelection)
        (let ((tag "PercentRepeat") (start-measurenum #f)(num_repeats (d-GetUserInput (_ "Percent Repeat") (_ "Give number of further repeats 1,2,...") "1")))
         
            (MoveToEndOfSelection)
            
            (if num_repeats
                (let ((numbering  (if (FullDurationMeasure?)  (d-GetUserInput (_ "Percent Repeat") (_ "Give frequency of numbering 0, 1,...") "1") #f))
                        (freq "\\set countPercentRepeats = ##f "))
                    (if numbering
                        (begin
                            (set! numbering (string->number numbering))
                            (if (and numbering (positive? numbering))
                                (set! freq (string-append  "\\set countPercentRepeats = ##t \\set repeatCountVisibility = #(every-nth-repeat-count-visible " (number->string numbering) ") ")))))
                        
                    (set! num_repeats (string->number num_repeats))
                    (d-Copy)
                    (d-PushPosition)
                    (MoveToSelectionBeginningInThisStaff)
                    (d-Directive-standalone tag)
                    (d-DirectivePut-standalone-minpixels tag 40)
                    (d-DirectivePut-standalone-postfix tag (string-append freq "\\repeat percent " (number->string (1+ num_repeats)) "{ "))
                    (d-DirectivePut-standalone-display tag (string-append "% " (number->string (1+ num_repeats))))
                    (d-DirectivePut-standalone-graphic tag (string-append "\nR{\nDenemo\n36"))
                    (d-DirectivePut-standalone-gy tag 10)
                    (d-PopPosition) ;;moves to where selection end was
                    
                    (d-MoveCursorRight) ;;we have inserted one object so when in the same measure we move right
                    (d-MoveCursorRight)
                    (d-Directive-standalone tag)
                    (d-DirectivePut-standalone-minpixels tag 60)
                    (d-DirectivePut-standalone-postfix tag "} %{ ")
                    (d-DirectivePut-standalone-graphic tag "\n}%{\nDenemo\n36")
                    (d-DirectivePut-standalone-gy tag 10)
     
                    (d-MoveCursorRight)
                    (let loop ((count num_repeats))
                        (if (FullDurationMeasure?)
                            (begin
                                (EnsureEmptyNextMeasure 'all)
                                (d-MoveToMeasureRight)))
                        (d-Paste)
                        (if (> count 1)
                            (loop (1- count))))
                    (d-Directive-standalone tag)
                    (d-DirectivePut-standalone-minpixels tag 40)       
                    (d-DirectivePut-standalone-postfix tag " %} ")
                    (d-DirectivePut-standalone-graphic tag "\n%}\nDenemo\n36")
                    (d-DirectivePut-standalone-gy tag 10)
                    (d-MoveCursorRight)
                    (d-RefreshDisplay)
                    (d-SetSaved #f))))
        (d-WarningDialog (_ "Cursor not in selection"))))
