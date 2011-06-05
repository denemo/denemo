#!(use-modules (srfi srfi-1)) ; List library. Needed for list-index
(define-module (actions denemo-modules commandlist)
	#:export (
		 CommandListScrollUp
		 CommandListScrollDown
	)) !#

#!
A set of functions to provide scrolling up and down through a list of commands. In other words: An extended toggle through multiple states.
 Intended to change a current preferences or existing object to another status. 
 Scroll through barlines to make a double barline an open repeat and after that a close repeat or vice versa.
 Scroll through the prevailing duration.
 Works optimal with a midi modwheel or other midi controllers. Minimal button/controller use for accessing multiple commands and states.

 CommandListScrollUp and CommandListScrollDown want a special list:
 Syntax: CommandList::UPPERCASE is a list of pairs: (key command/procname)
 Each key only once. The key must be something that can be retrieved by a Denemo d-Get... function. It can be any data type that works with (equal? ...)
 Each command should be mutaly exclusive: d-Set0 revokes d-Set2 and commands about a certain lilypond directive should take care of overwriting/deleting the old version first.
 Example, which copies d-MoveCursorUp in a more complex way :) Used as CursorUp this will do pentatonic steps. 
	(define CommandList::PENTATONIC (list
		(cons "c" d-MoveToC)
		(cons "d" d-MoveToD)
		(cons "e" d-MoveToE)
		(cons "g" d-MoveToG)
		(cons "a" d-MoveToA)))

		
	(CommandListScrollUp CommandList::CURSOR-Y (d-GetCursorNote))	
!#

;This list-ref version makes a normal list  a pseudo circular-list where you can give integers to list-ref out of the lists range. 
;; Supports integers up to double list length
;; Supports negative integers down to negative list length.
(define (CommandList:list-ref listy integer)
	(define listlength (length listy))
	(if (>= integer listlength) ; Support integer up to double length
		(set! integer (- integer listlength)))	
	(if (negative? integer)
			(set! integer (+ integer listlength))) ; support integer down to negative length.
	(list-ref listy integer))

(define (CommandListScrollUp commandlist current)
	;Find the current list position with list-index, increase the position and return a new value from the same list.
	(; execute the resulting command
		(cdr (CommandList:list-ref commandlist (1+ (list-index (lambda (x) (equal? (car x) current)) commandlist))))))

(define (CommandListScrollDown commandlist current)
	;Find the current list position with list-index, decrease the position and return a new value from the same list.
	(; execute the resulting command
		(cdr (CommandList:list-ref commandlist (1- (list-index (lambda (x) (equal? (car x) current)) commandlist))))))

;Definitions of lists
;;TODO: Scroll through different barlines. Problem: You still have to enter at least one. Maybe a higher level script can take care of inserting one.
;;;In the end: Write a wrapper function for single objects (no selections!!) that gets the type of an object and scrolls afterwards.

(define CommandList::PREVAILINGDURATION (list
	(cons -3072 d-SetBreve)
	(cons -6144 d-SetLonga)
	(cons 0 d-Set0)
	(cons 1 d-Set1)
	(cons 2 d-Set2)
	(cons 3 d-Set3)
	(cons 4 d-Set4)
	(cons 5 d-Set5)
	(cons 6 d-Set6)
	(cons 7 d-Set7)))
		
	
