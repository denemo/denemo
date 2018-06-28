;;AllowBreakAtHalfBar   
(d-GoToBeginning)
        (let ((ticks (/ (GetMeasureTicks) 2)))
        (while (d-NextNote)
            (if (eq? (d-GetStartTick) ticks)
                (begin 
                    (d-AllowLineBreak 'non-interactive)
                    (d-MoveCursorRight)))))
(d-DirectivePut-score-prefix "BarNumberingInterval" "\n\\layout {\\context {\\Score 
    barNumberVisibility = #(every-nth-bar-number-visible 1) 
    \\override Score.BarNumber.break-visibility = #begin-of-line-visible }}\n")                    