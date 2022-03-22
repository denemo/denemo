;;;DisplayPageAndSystemCounts
(let ((inc "page-systems-count.ily"))
	(d-LilyPondInclude (cons 'query inc)) ;;sets LilyPondInclude::return
	(if LilyPondInclude::return
		(d-LilyPondInclude (cons 'delete inc))
		(d-LilyPondInclude inc)))