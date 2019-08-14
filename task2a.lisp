(setf finlist '())

(write-line "Enter the list: ")
(setf mainlst (read))
(write-line "Enter the target element: ")
(setf goal (read))
(write-line "==========================================================")


; Main function that implements search
(defun bst(lst target counter)

		; Block that should be executed only once
		; Set all main variables
		(if (= counter 0)
			(progn
			(setf level 0)
			(setf returned nil)
			(setf lock 0))
		)
		
		; If fucnction reaches maximum number of recursive calls, returning
		; Max number of calls = max number of tree levels
		; This exit is used when target was not found
		(if (= counter (max-depth lst 0))
			(return-from bst 0)
		)
		
		; If function returns target element, return
    	(if (eq returned target)  			
        		(return-from bst 0)
        		
        )
        (progn		
        	; Setting deepest level to reach
        	(setf level1 (incf level))

        	; Lock is used to execute some statements exactly once during recursive calls
        	(if (= lock 0)
        		(setf returned (helper mainlst target level1 0))			
        		(setf returned (helper mainlst target level1 1))
        	)   	
			(setf lock 1)	; Lock = 1 means "locked" - there will be no access to some statements       			
 		)

 		(setf counter (+ counter 1))
 		(bst lst target counter)			; Next recursive call

)


(defun helper(lst target level lock)

	; Execute exactly once
	; Add the very first element to the list
	(if (= lock 0)
		(progn 
			(setf finlist (add-to-tail finlist (car lst)))
			(setf lock 1)
		)
	)
	; If target is found, return
	(if (eq target (car lst))
		(return-from helper target)
	)
	; If reach max given level, return
	(if (= level 0)
		(return-from helper 0)
	)
	; Going throung child elements
	(dolist (item (cdr lst))

		; Add elements to the list
		(setf finlist (add-to-tail finlist (car item)))
		(setf level1 (- level 1))
		(setf returned (helper item target level1 1))

		; Next recursive call
		(if (eq returned target)
			(return-from helper target)
		)
		; add parent to the list
		(setf finlist (add-to-tail finlist (car lst)))
	)
	(return-from helper 0)
)

; Calculate the max-depth of the tree - i.e. number of elements 
(defun max-depth (l counter)

		(if (= counter 0)
			(progn
			(setf tmplst '())
			(setf l (list l)))
		)

		(if (null l)
			(return-from max-depth (list-length tmplst)))
									
				(setq next-level nil)									; An empty list, where all child nodes will be placed

				; Going through list elements
				(dolist (item l) 

					; Process atom as a list, if element is atom
					(if (atom item) 
						(progn
							(setf item (list '() item))
							(setf item (cdr item))))

					(setf tmplst (add-to-tail tmplst  (car item)))		;Print current node
					; Put all child nodes in the 'next-level' list
					(mapcar #'(lambda (x) (and
											(setf next-level (add-to-tail next-level x))))  (cdr item))
				)

				(setf counter (+ counter 1))
				(max-depth next-level counter)							; Pass list with child nodes to the next recursion level
)


; Helper function that adds element tothe tail of the list
; Taken from source https://stackoverflow.com/a/13367974/9901274
(defun add-to-tail (l x)
   (reverse (cons x (reverse l)))
)

(defun prlist(lst)
	(dolist (el lst)
		(write el)
		(write-char #\Space)
	)
)

(bst mainlst goal 0)

(prlist finlist)