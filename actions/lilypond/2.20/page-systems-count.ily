%Adapted from Jean Abou Samra's code
\paper {
		page-post-process =
		  #(lambda (paper pages)
			 (let ((n-systems 0)
				   (page-min #f)
				   (page-max #f))
			   (for-each
				(lambda (page)
				  (for-each
				   (lambda (line)
					 (let ((sys (ly:prob-property line 'system-grob)))
					   (if (ly:grob? sys)
						   (let ((sys-page (ly:grob-property sys 'page-number)))
							(set! n-systems (1+ n-systems))
							(set! page-min (if page-min
											   (min page-min sys-page)
											   sys-page))
							(set! page-max (if page-max
											   (max page-max sys-page)
											   sys-page))))))
				   (ly:prob-property page 'lines)))
				pages)
			   (ly:warning "DenemoInfo=~a,~a" (1+ (- page-max page-min))  n-systems)))
       }
