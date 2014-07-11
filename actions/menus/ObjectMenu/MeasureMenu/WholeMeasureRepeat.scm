;;;WholeMeasureRepeat
(d-WholeMeasureRest)
 (d-DirectivePut-chord-gx "WholeMeasureRest" 40)
 (d-DirectivePut-chord-gy "WholeMeasureRest" 15)
(d-DirectivePut-chord-graphic "WholeMeasureRest" "
𝄎
Denemo
48")

(d-DirectivePut-chord-prefix "WholeMeasureRepeat" "\\once \\override MultiMeasureRest #'extra-offset = #'(0 . -1)  \\override MultiMeasureRest #'stencil  = #ly:multi-measure-rest::percent  \\override MultiMeasureRest #'thickness = #0.48 ")
(d-SetSaved #f)
;;;End of scheme script