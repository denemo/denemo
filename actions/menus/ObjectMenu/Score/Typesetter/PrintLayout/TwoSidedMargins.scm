;;;TwoSidedMargins
(let ((tag "TwoSidedMargins"))
	(if (d-Directive-paper? tag)
		(begin
			(d-DirectiveDelete-paper tag)
			(d-InfoDialog (_ "Single-sided margins will be used")))
             (d-DirectivePut-paper-postfix tag "
    two-sided = ##t
   inner-margin = 10\\mm   % scaled to paper-size
   outer-margin = 20\\mm   % scaled to paper-size
   binding-offset = 0\\mm  % scaled to paper-size
          "))
(d-SetSaved #f))
