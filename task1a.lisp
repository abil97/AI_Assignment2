(write-line "Enter branching factor: ")
(setf facta (read))
(write-line "Enter the depth: ")
(setf deptha (read))
(write-line "==========================================================")


(if (or (< facta 2) (< deptha 1))
	(progn
	(write-line "Branching factor should be > 2 and depth should > 0")
	(quit)
))

(defun make-a-list(fact depth)
	(setf lst (car (list-helper mainlst nil 1)))	
	(print lst)

)

; Main function that creates a list
(defun create-a-list(fact depth)
	(setf lst (generate (el-total-num depth fact)))
	(setf lst (car (list-helper lst nil 1)))
	lst

)

#| lst - original list to take elements from
	next-level - list to pass to the next level
	counter - counter for number of recursive calls |#
(defun list-helper (lst next-level counter) 

	(if (= counter 1)

	  (progn	
	  	;(format t "Current tree level: ~D~% " (- deptha counter))
		(setq num-to-take (powera facta (- deptha counter)))			; Count number of elements to take
		(setf tmplst (subseq lst (- (list-length lst) num-to-take)))	; Create a list of choosen elements

		; every element of list a ---> (a)
		(setf tmplst1 '())

	 	(mapcar #'(lambda (x) (setf tmplst1 (add-to-tail tmplst1 (atom-to-list x))                         
        	      )) tmplst)
	 	
		(setf tmplst tmplst1)
		(setq counter (+ counter 1))

		(setf lst (reverse lst))
		(setf lst (del-seq num-to-take lst))
		(setf lst (reverse lst))
	))


	(if (and (> counter 1) (< counter (+ deptha 1)))

		(progn
			(setq num-to-take (powera facta (- deptha counter)))			; Count number of elements to take
			
			;Create separate list of cars		
			(setf crlst (del-seq (- (list-length lst) num-to-take) lst)) ; Taking elements from original list
																		 ; Pass them to the new list
			(setf lst (reverse (del-seq num-to-take (reverse lst))))
			(setf num-of-iter (/ (list-length tmplst) facta))

			; Two temporary list used in the loop
			(setf tmplst1 '())
			(setf tmplst2  '())
	
			(dotimes (x num-of-iter)
					(dotimes (y facta)
						(setf tmplst2 (add-to-tail tmplst2 (nth y tmplst)))
					)
				(setf tmplst1 (add-to-tail tmplst1 tmplst2))
				(setf tmplst (del-seq facta tmplst))	
				(setf tmplst2  '())
			)
			(setf tmplst tmplst1)
			(dotimes (x num-to-take)
				(setf tc (elt crlst x))
				(setf tel (elt tmplst x))
				(push tc tel)
				(setf (nth x tmplst) tel)
			)
			(setq counter (+ counter 1))
			(list-helper lst tmplst counter)
		)
	)
	(setf returnl tmplst)
)

; Count total number of elements
(defun el-total-num (depth fact)
	(setq total (/ (- (powera fact depth) 1) (- fact 1)) )
)

; Calculate the power of number
; Taken from source https://stackoverflow.com/a/29635182/9901274
(defun powera(base exp)
 ( if (eq exp 0) 
 	1 
 	( * base (powera base (- exp 1)) ) )
)

; Convert atom to list
(defun atom-to-list (at)
	(setf l (list '() at))
     (setf l (cdr l))
)

;======================================================================
; GENERATOR FUNCTIONS
;======================================================================


; Delete sequence 
(defun del-seq(end lst)
	(dotimes (x end lst)
		(setf lst (remove (car lst) lst))
	)
)

; Convert string to list. Helper function
; Taken from source:  https://stackoverflow.com/a/13832673/9901274
(defun string-to-list (str)
        (if (not (streamp str))
           (string-to-list (make-string-input-stream str))
           (if (listen str)
               (cons (read str) (string-to-list str))
               nil)))

; Generate list of numbers
(defun generate(num)

	(setf alphabet '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))

	(cond 
		((< num 27) (setf result alphabet))
		((< num 703) (setf result (append alphabet (generate-double-list (- num 26)))) )
		(t    (setf result (append alphabet (generate-double-list 676) (generate-triple-list (- num 702)))))

	)
	(setf result1  (subseq result 0 num))
	result1
)

; Generate one double pair
(defun generate-double-pair (fst snd)

	(setf fst (string fst))
	(setf snd (string snd))
	(setf str (concatenate 'string fst snd))
	(setf str (string-to-list str))
	(setf str (car str))
)

; Generate list o double pairs
(defun generate-double-list (num)

	(setf alp1 '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))
	(setf gen '())

	(dotimes (x 26)
		(dotimes (y 26)
			(setf fst (elt alp1 x))
			(setf snd (elt alp1 y))
			(setf gen (add-to-tail gen (generate-double-pair fst snd)))
			(if (= (list-length gen) num)
				(return-from generate-double-list gen)
			)
		)
	)
	gen
)

; Generate onee triple pair
(defun generate-triple-pair (fst snd trd)

	(setf fst (string fst))
	(setf snd (string snd))
	(setf trd (string trd))
	(setf str (concatenate 'string fst snd trd))
	(setf str (string-to-list str))
	(setf str (car str))

)

; Generate list of triple pairs
(defun generate-triple-list (num)

	(setf alp1 '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))
	(setf gen '())
	(dotimes (x 26)
		(dotimes (y 26)
			(dotimes (z 26)
				(setf fst (elt alp1 x))
				(setf snd (elt alp1 y))
				(setf trd (elt alp1 z))
				; Add generated pairs to the list
				(setf gen (add-to-tail gen (generate-triple-pair fst snd trd)))
				(if (= (list-length gen) num)
					(return-from generate-triple-list gen)
				)
			)
		)
	)					
	gen
)

; Helper function that adds element tothe tail of the list
; Taken from source https://stackoverflow.com/a/13367974/9901274
(defun add-to-tail (l x)
   (reverse (cons x (reverse l)))
)

(print (create-a-list facta deptha))