

(write-line "Enter the list: ")
(setf mainlst (read))
(write-line "Enter the target element: ")
(setf goal (read))
(write-line "==========================================================")


(setf res '())
(setf found 0)
(setf lock 0)

; Main function that runs the program
(defun run()

	(setf x (dfs mainlst mainlst goal))
	(if (= x 0)
		(progn
		(setf res (list-without-last res))
		(prlist res))
	)
	(if (= x 1)
		(prlist res)
	)
)

(defun prlist(lst)
	(dolist (el lst)
		(write el)
		(write-char #\Space)
	)
	(write goal)
)

; function that implements serch recursively
(defun dfs (lst ltr target)
	; Execute this block exactly once
	(if (= lock 0)
		(progn 
			(setf lst (list lst))
			(setf lock 1)
		)
	)
	; Termination condition
	(if (null lst)
		(return-from dfs 0)
	)
	; If element is found, return
	(if (= found 1)
		(return-from dfs 1)
	)

	; Going through all list elements
	(dolist (el lst)
		
		; Add element to the list
		(setf letter (car el))
		(setf res (add-to-tail res letter))

		; It targetis found, return
		(if (eq target letter)
			(progn 
				(setf found 1)
				(return-from dfs 1)
			)
		)
		; Recursive call
		(dfs (cdr el) letter target)

		; Add parent to list
		(if (= found 0)
			(setf res (add-to-tail res ltr))
		)
	)
	(return-from dfs 0)
)


; Helper function that adds element tothe tail of the list
; Taken from source https://stackoverflow.com/a/13367974/9901274
(defun add-to-tail (l x)
   (reverse (cons x (reverse l)))
)

; Helper function that deletes last element
; taken from source: https://stackoverflow.com/a/22974770/9901274
(defun list-without-last (l)
  (if (> (length (rest l)) 0)
      (append (list (first l)) (list-without-last (rest l)))
      nil))

(run)