;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
  ;;;;;; FindEditPoint
(let loop ( )
  (define current (d-PrevNote))
  (if (and current (d-GetNonprinting))
      (loop)))
 (if (not (d-GetNonprinting))
(d-NextNote))
(d-RefreshDisplay)
;;;;;;;;;;;;;;;;;

