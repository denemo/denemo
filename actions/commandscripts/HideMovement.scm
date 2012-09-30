;;;HideMovement
(if (d-DirectiveGet-movementcontrol-display "HideMovement" )
(begin
 (d-DirectiveDelete-movementcontrol "HideMovement")
      (d-DirectiveDelete-layout "HideMovement")
      (d-DirectiveDelete-header "HideMovement"))
 (begin
      (d-DirectivePut-movementcontrol-display "HideMovement" (_ "NO PRINT"))
      (d-DirectivePut-movementcontrol-override "HideMovement" (logior DENEMO_OVERRIDE_TAGEDIT DENEMO_OVERRIDE_GRAPHIC DENEMO_OVERRIDE_LILYPOND))
      (d-DirectivePut-movementcontrol-postfix "HideMovement" "HiddenMovement = { ")
      (d-DirectivePut-layout-override "HideMovement" DENEMO_OVERRIDE_LILYPOND)
      (d-DirectivePut-header-override "HideMovement" DENEMO_OVERRIDE_LILYPOND)))
(d-RefreshDisplay)
