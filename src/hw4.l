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