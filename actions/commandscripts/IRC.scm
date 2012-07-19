;;; IRC on freenode
 (d-InfoDialog "Your browser may take a few moments to start - be patient!
 	When it has started you have to type in some anti-spam words
 	and click connect
 	Then you get a page with a place to type your question at the bottom.
 	Type hello in there and hit Enter
 	If no one is there to help you can leave an email address with your questions.
 	Or use the mailing list...")
(d-Help (string-append "http://webchat.freenode.net/?nick=" (d-GetUserName) "_" DENEMO_VERSION "&channels=denemo"))