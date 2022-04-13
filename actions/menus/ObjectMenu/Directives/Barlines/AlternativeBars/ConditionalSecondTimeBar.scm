;ConditionalSecondTimeBar
;Insert 2nd time bar for other print-outs only
(if (d-HasSelection) 
     (let ((choice (RadioBoxMenu 
                        (cons  (_ "Duplicate the selected music\nfor use as second time bar(s)\nwhen not typesetting this part on its own") 'ok))))
          (case choice
                ((else)
                    (d-UnsetMark))
                ((ok)
                    (d-Copy)
                    (while (d-IsInSelection) (d-MoveCursorRight))))))
(if (not (d-HasSelection))
        (let ((choice (RadioBoxMenu 
                        (cons  (_ "Duplicate from cursor to end\nfor use as second time bar(s)\nwhen not typesetting this part on its own") 'ok))))      
            (case choice
                ((ok)
                    (d-SetMark)
                    (d-GoToEnd)
                    (d-Copy)))))
(if  (d-HasSelection)
        (let ((choice (RadioBoxMenu (cons (_ "End of Movement") 'end) (cons (_ "Mid-Movement") 'mid))))
            (case choice
                    ((end) (d-RepeatEnd))
                    ((mid) (d-RepeatEndStart)))
            (d-MoveCursorLeft)
            (d-OnlyForLayout (cons 'noninteractive (cons (d-StaffProperties "query=lily_name") (d-GetCurrentStaffLayoutId))))
            (d-MoveCursorRight)
            (if (Appending?)
                (d-InsertMeasureAfter)
                (begin
                	(d-SplitMeasure)
			(d-MoveCursorLeft)
                	(d-ShortMeasure)))
            (d-IgnoreOn 'noninteractive)
            (d-OnlyForLayout (cons 'noninteractive (cons (d-StaffProperties "query=lily_name") (d-GetCurrentStaffLayoutId))))
            (d-MoveCursorRight)
            (d-Paste)
            (d-IgnoreOff 'noninteractive)
            (d-OnlyForLayout (cons 'noninteractive (cons (d-StaffProperties "query=lily_name") (d-GetCurrentStaffLayoutId))))
            (d-InfoDialog (_ "Now place first and second time markings in the part(s) that have distinct first and second time bars"))))




