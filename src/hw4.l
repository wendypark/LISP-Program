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
    ((NULL list_num) (LIST 10000 -10000 ))
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



(defun before (begin end list_input)
	(cond 
    ; Return empty list
		((null list_input) nil)
    ; if equals to begin
    ((EQ (CAR list_input) begin)
      (LIST (CAR list_input) before(begin end (CDR list_input)))
      )
    ; if equals to end, end recursion
    ((EQ (CAR list_input) end)
      end
      )
	)
)
