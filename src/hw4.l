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
    ((NULL list_num) (LIST 1000 -1000 ))
    ((ATOM list_num)
      (LIST list_num list_num)
    )
    (T
      (LIST
        (min (CAR list_num) (CAR(range (CDR list_num))) )
        (max (CAR list_num) (CAR(CDR(range (CDR list_num)))) )
      )
    ) 
  )
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

