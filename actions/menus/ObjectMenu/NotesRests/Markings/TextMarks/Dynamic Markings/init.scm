 ;;;;;;;;;;; dynamics init.scm
 (define* (Dynamics::Put graphic lily   #:optional (midi-vol #f))
(d-DirectivePut-chord-graphic "Dynamic"  graphic)
(d-DirectivePut-chord-gx "Dynamic"  -10)
(d-DirectivePut-chord-gy "Dynamic"  50)
(d-DirectivePut-chord-postfix "Dynamic"  lily)
(d-DirectivePut-chord-minpixels  "Dynamic" 20)
(if (string? midi-vol)
(begin
  (d-DirectivePut-chord-override "Dynamic" (logior DENEMO_OVERRIDE_VOLUME DENEMO_OVERRIDE_STEP))
  (d-DirectivePut-chord-midibytes "Dynamic" midi-vol)))
(d-RefreshDisplay))