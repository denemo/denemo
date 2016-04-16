(use-modules (ice-9 q))

#!(define-module (actions denemo-modules helpsystem)
    #:export (
         Help::RemoveTag
         Help::TimedNotice 
         Help::Push
         Help::Pop
    )) !#


; This help system is designed to give out messages in the second Denemo status bar.
;; You can push and pop to a message-queue. If you Pop the front message the one before that will appear again
;; Each help message consists of a tag-symbol and a help-string. Only one message of each tag is allowed in the queue. If you push a another message with an existing tag the old message gets deleted.
;; Please stick to the following tag convention
;;; 'timednotice - for a message that will delete itself after a few seconds (you might want use (Help::TimedNotice) anyway)
;;; 'doublestroketemp - for any message that tells the user that a command waits for another keypress, like a duration key.
;;; 'doublestroke - a permanent message that shows the current keybindings or give permanent info about some special mode you might be in.

(define Help::queue (make-q)) ; Prepare the message queue. Only one is used for the entire system.

(define (Q-remove! q obj)  ; A function that replaces Guiles q-remove!. Ours works with a pair and only looks for the car.
    (set-car! q (filter (lambda (x) (not (equal? (car x) obj))) (car q)))
    (sync-q! q))

(define (Help::RemoveTag tag) ; Remove all messages with this 'tag symbol
    (Q-remove! Help::queue tag)
    (Help::UpdateWriteStatus))

(define (Help::ClearQueue) ; Clear the entire queue
    (set! Help::queue (make-q))
    (Help::UpdateWriteStatus))

;Take the current front message and display it.
;;Help::UpdateWriteStatus is the only function that accesses the StatusBar.
;;If there is no message left in the queue, disable showing the message area.
(define (Help::UpdateWriteStatus)
    (if (q-empty? Help::queue)
        (d-WriteStatus "                                  ") ;;; keep the status bar visible
        (d-WriteStatus (cdr (q-front Help::queue)))))

;Push a message to the queue. 
;; Also Updates the StatusBar through UpdateWriteStatus
(define (Help::Push pair)
    (Help::RemoveTag (car pair)) ; remove all with the same tag first. TODO: evaluate if this is a heavy performance-loss   
    (q-push! Help::queue pair)
    (Help::UpdateWriteStatus))

;Remove the current front message from the queue
;; Also Updates the StatusBar through UpdateWriteStatus
(define (Help::Pop)
    (if (not (q-empty? Help::queue))
        (begin
            (q-pop! Help::queue)
            (Help::UpdateWriteStatus))))

; Display a message that deletes itself after timing (default 2500ms). No tag or Pop needed from the user.
(define* (Help::TimedNotice string #:optional (timing 2500))
    (Help::Push (cons 'timednotice string))
    (Help::Push (cons 'timednotice1 (string-append string "\n\n\n")))
     (Help::Push (cons 'timednotice2 (string-append string "\n\n\n")))
     (Help::Push (cons 'timednotice3 (string-append string "\n\n\n\n")))
     (Help::Push (cons 'timednotice4 (string-append string "\n\n\n\n\n")))
     (let ((at 0) (step 250))
        (set! at (+ at step))
        (d-OneShotTimer at "(Help::Pop)")
        (set! at (+ at step)) 
        (d-OneShotTimer at "(Help::Pop)")
        (set! at (+ at step))
        (d-OneShotTimer at "(Help::Pop)")        
        (set! at (+ at step))
        (d-OneShotTimer at "(Help::Pop)")        
        (set! at (+ at step))
  
        (d-OneShotTimer (* 2 timing) "(Help::Pop)")))
    

(define* (TimedNotice string #:optional (timing 2500))
    (Help::TimedNotice (string-append  "<span font_desc=\"16\" foreground=\"blue\">" string   "</span>") timing))
    

