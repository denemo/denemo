;;;EmailForHelp

(let ((text (d-GetMultilineUserInput (_ "Help by Email") (_ "Please explain the problem you are having") "My operating system version is ...")))
	(define newline-escape (string-escaper '((#\newline . "  "))))
	(define break-escape (string-escaper '((#\newline . "<br>"))))
	(if (string? text)
		(begin
			(set! text (string-append  "
				<!DOCTYPE html>
				<html>
				<head>
					<title>Denemo Help by Email</title>
				</head>
				<body>
					<h1>Denemo Help by Email</h1>
					<p>The link here will launch your email program with your question in it</p>
					<a href=\"mailto:denemo-devel@gnu.org?subject=Help with Denemo Version " DENEMO_VERSION "&body=" (newline-escape (html-escape text)) "\">Send email to Denemo</a>
					<p>You can edit the question before you send the email</p>
					<p>If the email client does not launch then start your email program yourself and send you message to denemo-devel@gnu.org</p>
					<p>This is the text of your email, should you need to copy and paste it in that case<p>
					<p>" (break-escape (html-escape text)) "<p>
				</body>
				</html>								
			"));
			(d-EmailHelp text)
			(d-InfoDialog   (string-append (_ "Your browser will now start with a button to send your email. If the email form fails to show, send your email to denemo-devel@gnu.org"))))))
