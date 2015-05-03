 ;;;InsertNthSnippet
(Help::Push (cons 'doublestroketemp
(string-append " <span font_desc=\"22\" foreground=\"blue\">"  (_ "Type Snippet Number 1,2...")  "</span>")))
(let ((duration (d-GetKeypress)))
(if (and duration  (string->number duration))
   	(begin 
		(d-InsertSnippet (string->number duration) #f)
		(Help::Pop))
  	(begin (Help::Pop) (Help::TimedNotice (_ "To use this function correctly you need to give the number of the snippet to be inserted")))))
