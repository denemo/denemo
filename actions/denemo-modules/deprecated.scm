#!(define-module (actions denemo-modules deprecated)) !#

;;;;;;;;;;;;;; Refresh Procedures.
;;;;;;;;;;;;;; Naming convention D-<tag> is the refresh proc for tag

(define (D-Anacrusis)
(let ((duration (d-GetDurationInTicks)))
  (if (boolean? duration)
      (set! duration 0))
  (while (d-NextObjectInMeasure) 
	 (set! duration (+ duration (d-GetDurationInTicks))))
  (PrevDirectiveOfTag "Anacrusis")
  (set! duration (/ duration 12))
  (if (equal? 0 duration)
      (begin
	(HideStandaloneDirective))
      (begin
	(d-DirectivePut-standalone-postfix "Anacrusis" (string-append "\\partial 128*" (number->string duration) " " ))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; This is Denemos interface to access the MediaWiki API (http://www.mediawiki.org/wiki/API), which is used for the current Denemo-Website
;;;; Send any question to Nils Gey denemo@nilsgey.de
;;;; Currently its only used to create/overwrite a page with a new script.
;;;; It uses the User-Rights System so its very secure. Vandalism in only possible in the same degree as allowed on the website itself.
;;;; All API access is done via (d-HTTP). The C function behind it sends HTTP-POST data to the given Server/Website and returns the HTTP-header and MediaWiki Data. 
;;;; The basic steps are 1)Login with Username/PW given in Denemos Preferences and 2)Create a HTTP-Cookie .
;;;; After that allowed Manipulation is possible. Currently we create request an Edit-Token and create a new Page.

(define (d-UploadRoutine list)
	  (define command (list-ref list 0))
	  (define name (list-ref list 1))
	  (define script (list-ref list 2))
	  (define initscript (list-ref list 3))
	  (define menupath (list-ref list 4))
	  (define label (list-ref list 5))
	  (define tooltip (list-ref list 6))
	  (define after (list-ref list 7))

	  ;Some constants. Change these only if the Website moves.
	  (define HTTPHostname "www.denemo.org") ; don't use http:// . No tailing /
	  (define HTTPSite "/api.php")   
		
		; Prepare Login. Use this only once in (CookieString) because all tokens change on any new login.
		(define (LogMeIn) 
				(d-HTTP ;Parameters are hostname, site, cookies/header and POST-body
				HTTPHostname
				HTTPSite
				"" ; Cookie entrypoint. No Cookie for now.
				(string-append "format=json&action=login&lgname=" (scheme-escape(d-GetUserName)) "&lgpassword=" (scheme-escape(d-GetPassword)))))

		; Actually logs you in and prepares a HTTP-Cookie you have to use in all other Media-Wiki Actions as third (d-HTTP) parameter.
		(define (CookieString) 
			(define LogMeInReturn (LogMeIn))
			
				; Raise Error. Sorry, I don't know how to make Blocks and if/else does only allow one statement.
				(define	(RaiseError)
				  (begin
					(d-WarningDialog "Login Error - Please check your username and password in Edit->prefs->misc")
					(display "\nLogin Error - Please check your username and password in Denemos Preferences")
					;return CookieError
					(string-append "CookieError"))		
				)
				
				; Test if hostname is ok
				(if (string-ci=? LogMeInReturn "ERROR")
				(display "\nConnection Error - Server unavailable")
				
					;If Server is ok check Login-Data:
					(if  (string-ci=? (ParseJson LogMeInReturn "result") "Success")
							
					; If login is good go ahead and build the cookie string						
					(string-append 
					"Cookie: "(ParseJson LogMeInReturn "cookieprefix")"UserName=" (ParseJson LogMeInReturn "lgusername")
					"; "(ParseJson LogMeInReturn "cookieprefix")"UserID=" (ParseJson LogMeInReturn "lguserid") 
					"; "(ParseJson LogMeInReturn "cookieprefix")"Token=" (ParseJson LogMeInReturn "lgtoken")
					"; "(ParseJson LogMeInReturn "cookieprefix")"_session=" (ParseJson LogMeInReturn "sessionid") 
					"\n")					
					;else
					(RaiseError))))	

		; Prepare request Edit-Token.
		; First send d-HTTP, then parse the token, then modify it to the right format.
		(define (GetEditToken name CookieStringReturn)
			(define (ReceiveRawToken)
				(d-HTTP 
				HTTPHostname
				HTTPSite
				CookieStringReturn
				(string-append "format=json&action=query&prop=info|revisions&intoken=edit&titles="name)))	
			
			;json gives you +\\ @ Tokens end, but you need only +\ which is %2B%5C in url-endcoded format. 
			(string-append (string-trim-both (string-trim-both (ParseJson (ReceiveRawToken) "edittoken" ) #\\) #\+) "%2B%5C"))	
		
		;This will overwrite the page named like the parameter "name". If it is not existend it will be created.
		;Any OverwritePage call has to be made in (d-UploadRoutine)'s body.
		(define (OverwritePage CookieStringReturn)
			
			(define (GetLicenseAndBuildString)
				;(define license (d-GetUserInput "License" "Please choose a license for your script. For example GPL or LGPL" "GPL")) ; This is gone. Scripts have to be GPL, too.
				(define (SiteString) ; Any whitespace will be send, too.
	(string-append 
	"{{Script
	|Name = " name "
	|Author =  " (scheme-escape(d-GetUserName)) "  
	|Label = " label  "
	|License =  GPL 
	|Explanation = " tooltip "
	|SubLabel = " menupath "
	|Version = " DENEMO_VERSION "
	}}
	=== Script ===			
	<syntaxhighlight lang=\"scheme\">
	" script "
	</syntaxhighlight>
				
	=== Initscript === 
	<syntaxhighlight lang=\"scheme\">
	" initscript "
	</syntaxhighlight>
				
	=== After === 
	<syntaxhighlight lang=\"scheme\">
	" after "
	</syntaxhighlight>
	"))
				
				;Send the data to let the API generate a new site!		
				(d-HTTP
					HTTPHostname
					HTTPSite
					CookieStringReturn
					(string-append "action=edit&title=" name "&format=json&summary=" tooltip "&text=" (SiteString) "&token=" (GetEditToken name CookieStringReturn)))	
				
				;Show script in browser
				(d-Help (string-append "http://" HTTPHostname "/index.php/" name))				
		
			); End of GetLicenseAndBuildString
		
		
		;check if Login/Building the Cookie was correct
		(if (string-ci=? CookieStringReturn "CookieError")
			(display "\nAn error occurred while performing the task. Thats why the result of your Upload-Command is: ")
			(GetLicenseAndBuildString))
		);;;; End of OverwritePage 
		
		;;;; The real action happens here.	This is the only place where (CookieString) is called so we have only one Login at all.
		(display (OverwritePage (CookieString))) ;show and execute

	) ; End Of (d-UploadRoutine)
