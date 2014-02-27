;;MergeRests
(let ((tag "MergeRests"))
  (if (d-Directive-layout? tag)
	(begin
		(if (not MergeRests::params)
			(d-InfoDialog (_ "Turning off merging of rests, use Merge Rests command to turn it on")))
		(d-DirectiveDelete-layout tag))
	(begin
	 (d-LilyPondInclude "merge-rests.ily")
	(d-DirectivePut-layout-postfix tag " \\context {
    \\Staff
    \\override RestCollision #'positioning-done = #merge-rests-on-positioning
    \\override MultiMeasureRest #'Y-offset = #merge-multi-measure-rests-on-Y-offset
  }" ))))
(d-SetSaved #f)