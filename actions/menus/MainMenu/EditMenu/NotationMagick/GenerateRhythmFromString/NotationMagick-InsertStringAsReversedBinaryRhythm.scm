(let ((return (NotationMagick::AskNewAsciistring)))
	(if return
		(NotationMagick::PutBinaryStringList (NotationMagick::ReverseStringsInList (NotationMagick::String->CharsAsBinary return)))
		#f))