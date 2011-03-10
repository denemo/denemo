(use-modules (ice-9 q))

(define (Q-remove! q obj) 
	(set-car! q (filter (lambda (x) (not (equal? (car x) obj))) (car q)))
	(sync-q! q))

(define (Help::RemoveTag tag)
	(Q-remove! Help::queue tag))

(define Help::queue (make-q))

;Help::UpdateWriteStatus is the only function that accesses the StatusBar.
(define (Help::UpdateWriteStatus)
	(if (q-empty? Help::queue)
		(d-WriteStatus)	
		(d-WriteStatus (cdr (q-front Help::queue)))))

(define (Help::Push pair)
	(Help::RemoveTag (car pair)) ; remove all with the same tag first. TODO: evaluate if this is a heavy performance-loss	
	(q-push! Help::queue pair)
	(Help::UpdateWriteStatus))

(define (Help::Pop)
	(q-pop! Help::queue)
	(Help::UpdateWriteStatus))
