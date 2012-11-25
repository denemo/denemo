;;; Warning!!! This file is derived from those in actions/menus/... do not edit here
;;;SetMovementTempo
(define MovementTempo:value (number->string (d-MovementTempo)))
(set! MovementTempo:value (d-GetUserInput (_ "Tempo of Movement") (_ "Give quarter notes per minute:") MovementTempo:value))
(set!  MovementTempo:value (number->string (d-MovementTempo MovementTempo:value)))
(d-RefreshDisplay)
