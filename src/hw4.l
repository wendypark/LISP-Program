;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Your solution goes in this file ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun all-length (top_list)
 	; "counts the number of elements at all levels of a list"
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