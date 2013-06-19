(let ((return (NotationMagick::AskNewAsciistring)))
	(if return
		(NotationMagick::PutBinaryStringList (NotationMagick::String->CharsAsBinary return))
		#f))