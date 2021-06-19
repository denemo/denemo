;;; IRC on freenode
 (d-InfoDialog (_ "Your browser may take a few moments to start - be patient!
 	When it has started click Start
 	Then you get a page with a place to type your question at the bottom.
 	Type hello in there and hit Enter
 	If no one is there to help you can use the mailing list denemo-devel@gnu.org"))
(d-Help (string-append "https://web.libera.chat/?nick=" (d-GetUserName) "_" ((string-escaper '((#\. . "_"))) DENEMO_VERSION) "#denemo"))
