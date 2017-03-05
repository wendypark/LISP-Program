;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Your solution goes in this file ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(DEFUN all-length (L)
  (COND 
    ((NULL L) 0)
    ((ATOM (CAR L))
      (COND
        ((NULL (CAR L))
        	( +
        		0
        		(all-length (CDR L))
        		);+
        	);((NULL...
        (T
        	( +
        		1
        		(all-length (CDR L))
        		);+
        	);T
        );COND
    )
    (T 
    	( +
    		(all-length (CAR L))
    		(all-length (CDR L))
    		);+
    	);T
  )
)

(DEFUN range (NEL)
  (COND
    ((NULL NEL) (LIST nil nil))
    ((EQUAL (LENGTH NEL) 1) (LIST (CAR NEL) (CAR NEL)))
    (T (LIST (MIN (CAR NEL) (CAR (range(CDR NEL)))) (MAX (CAR NEL) (CADR(range(CDR NEL)))) ))  
  )
)

(DEFUN before (A1 A2 L)
  (COND
  	((NULL L) NIL)
    ((EQUAL A1 (CAR L))
      (COND
        ((MEMBER A2 (CDR L)) (CONS A1 (before_helper A2 (CDR L))))
        (T (CONS A1 (CDR L)))
      )
     );eq
    (T (before A1 A2 (CDR L)));else
  );COND
);defun


(DEFUN before_helper (A2 L)
  (COND
    ((EQUAL A2 (CAR L)) (LIST A2))
    (T
      (CONS (CAR L) (before_helper A2 (CDR L)))
      )
  )
)

(DEFUN split-if (F L)
(COND
	((NULL L) (LIST NIL NIL))
	(T 
	    (DO ( ;variables to bind, what their initial values are, and how to update them
		        (FINAL L (CDR FINAL)) ;iterator 
		        (F_L NIL ;empty list
		        	(COND 
		        		( (NOT(FUNCALL F (CAR FINAL)))  (APPEND F_L (list (CAR FINAL)))) 
		        		(T F_L)
		        	);cond
		        );F_L
		        (T_L NIL ;empty list
		        	(COND 
		        		((FUNCALL F (CAR FINAL)) (APPEND T_L (LIST (CAR FINAL)))) 
		        		(T T_L)
		        	);cond
		        );T_L
	        )
	        ((NULL FINAL) (LIST F_L T_L));termination condition and return value 
	    )
	)
)
)


(DEFUN group (L N)
  (COND
    ((NULL L) (LIST NIL))
    ((EQUAL N 1) 
    	(DO (
    		  (L2 L (CDR L)) ;iterator
	          (FINAL NIL (APPEND FINAL (LIST (LIST (CAR L)))))
            )
            ((NULL L2) FINAL);terminate when L2 is empty, return FINAL list
        )
    )
    (T
    	(DO 

    	)

    )

  )

)

(DEFUN mostn (F L)

)