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



; Main function that builda the list
(defun build-list (fct dpt)

	(setf totally (el-total-num deptha facta))
	(setf lst (generate totally))
    (setf coun 1)
	(setf result '())
	(setf result (add-to-tail result (car lst)))
	(setf lst (cdr lst))
	(setf result (append result (func coun fct dpt)))
	result
)

; Helper function that goes through list recursively
(defun func(coun fct dpt)

	(incf coun)
	(if (= coun dpt)
		(progn
		(setf res '())
		(dotimes (x fct)
			(setf res (add-to-tail res (list (car lst))))
			(setf lst (cdr lst))
		)		
		(return-from func res))
	)
	(setq result '())

	(dotimes (x fct)
		(setf curr-el (list (car lst)))
		(setf lst (cdr lst))
		(setf result (add-to-tail result (append curr-el (func coun fct dpt))))
	)
	result
)


;Calculate total number of elements
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

;==========================================================================================
; GENERATOR FUNCTIONS 
;==========================================================================================


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



(print (build-list facta deptha))