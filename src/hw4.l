;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Your solution goes in this file ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun all-length (top_list)
  (COND
    ( (NULL top_list) 0)
    ( (ATOM(CAR top_list))
      (COND 
        ((NULL(CAR top_list)) (+ 0 (all-length (CDR top_list)) ))
        (T (+ 1 (all-length (CDR top_list)) ))
      )
    )
    (T (+ (all-length (CAR top_list))
          (all-length (CDR top_list))))
  )
)

(defun range (list_num)

  (COND
    ((NULL list_num) NULL)
    ((< (CAR list_num) (range (CDR list_num)))  ) 

  )

 	; "takes a list of numbers with at least one element and returns list of length 2 of smallest and largest numbers"
 	(list (apply 'min list_num) (apply 'max list_num))
)



(defun before1 (begin end list_input)
	(cond 
		((null list_input) nil)
		(if (equal begin (car list_input))
			(if (member end '(list_input))
				(butlast (list_input) (end))
				(before begin (cdr list_input))
			))
	)
)

(defun before2 (begin end list_input)
	(cond ((null list_input) nil)
	((equal begin (car list_input)) )
	((member end '(list_input)) (butlast (end list_input)))
	(t (before begin (cdr list_input)))
	)
)

(defun before3 (begin end list_input)
	"searches a list and returns a list of all elements in between the first two, inclusive"
	(COND ;condition
    ( (NULL list_input) 0) ;if list is empty return 0
    ( (equal begin (car list_input))) ;if first element of list is an ATOM
      (COND ;conditions
        ((member end '(list_input)) (butlast (list_input) (end)))  
        (T (nil))
      )
    )
    (T (before begin end (cdr(list_input)))
  )
)
 


(defun split-if (function list_input)

)

(defun group (list_input num)

)

(defun mostn (function list_input)

)

; "3. Write a function before that searches a list and 
; returns a list of all elements in between the first two arguments (inclusive)."

; (defun before (a b L)
;   (push
;     b
;     (cdr
;       (loop for i in L
;         until (EQ i b)
;         collect i)
;     )
;   )
; )

