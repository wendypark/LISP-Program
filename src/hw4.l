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
		        		( (NOT(FUNCALL F (CAR FINAL)))  (APPEND F_L (LIST (CAR FINAL)))) 
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
 
(DEFUN group (list_input n)
  (COND
    ((NULL list_input)
      NIL
      );((NULL
    (T
      ; Recurse on all but last element of list
      (let ((result (group (BUTLAST list_input) n)))
        (COND
          ; If result.last == NIL OR result.last.length >= n
          ((OR
              (NULL (last result))
              (>= (LENGTH (last result)) n)
              );OR
            (APPEND
              result
              (LIST (LIST (CAR (last list_input))))
              );APPEND
            );((OR
          (T
            (APPEND
              ; Previous result - result.last
              (BUTLAST result)
              ; result.last + new value
              (APPEND
                (last result)
                (last list_input)
                );APPEND
              );APPEND
            );T
          );COND
        );let
      );T
    );COND
  );defun

(DEFUN mostn (F L)
  (COND 
    ((NULL L) (LIST NIL))
    (T 
      (DO (
            (L2 L (CDR L2)) ;iterator 
            (SUBLIST NIL 
              (COND 
                ((EQUAL (highest_score F L) (FUNCALL F (CAR L2))) (APPEND SUBLIST (LIST (CAR L2)))) 
                (T SUBLIST)
              );inner cond
            );sublist declaration
          );do 
          ((NULL L2) (APPEND (LIST SUBLIST) (LIST (highest_score F L))))
      );do
    );else
  );cond
);DEFUN


(DEFUN highest_score (F L) 
  (DO (
        (L2 L (CDR L2)) 
        (final (FUNCALL F (CAR L)) 
          (COND 
            ((> (FUNCALL F (CAR L2)) final) (FUNCALL F (CAR L2)))
            (T final)
          )
        )
      )
      ((NULL L2) final)
  )
)

(DEFUN match (pattern assertion)
  (COND
    ; End of recursion -- True
    ((AND (NULL pattern) (NULL assertion))
      T
      );(AND

    ; If any lists ended early -- False
    ((OR (NULL pattern) (NULL assertion))
      NIL
      );(AND

    ; Raw elements are equivalent -> Check if tail is True
    ((EQUAL (CAR pattern) (CAR assertion))
      (match (CDR pattern) (CDR assertion))
      );(EQUAL

    ; Special case: ?
    ((EQUAL '? (CAR pattern))
      ; Just skip ahead once
      (match (CDR pattern) (CDR assertion))
      )

    ; Special case: !
    ((EQUAL '! (CAR pattern))
      (match_helper pattern assertion)
      )
    );COND
  );defun

; This only checks for case of !
; NOTE: pattern - includes the first !
(DEFUN match_helper (pattern assertion)
  (COND
    ; Assertion ended early
    ((NULL assertion) Nil)

    ; Case: ! is end of pattern
    ((NULL (CDR pattern))
      T
      );(NULL

    ; Case: ! term
    (T
      ; Try all cases where assertion moves forward
      ;   Keep going ahead in pattern until matching next member
      ;   Keep going ahead in pattern until next next matching next member
      ;   ... repeat until end
      (OR
        (match (CDR pattern) (CDR assertion))
        (match_helper pattern (CDR assertion))
        )
      )
    );Cond
  )

(DEFUN match-var (pattern assertion)
  (COND
    ; End of recursion -- True
    ((AND (NULL pattern) (NULL assertion))
      T
      );(AND

    ; If any lists ended early -- False
    ((OR (NULL pattern) (NULL assertion))
      NIL
      );(AND

    ; Raw elements are equivalent -> Check if tail is True
    ((EQUAL (CAR pattern) (CAR assertion))
      (match-var (CDR pattern) (CDR assertion))
      );(EQUAL

    ; Special case: ?
    ((EQUAL '? (CAR(CAR pattern)))
      (SET (CAR(CDR pattern)) (CAR assertion))
      )





  )
)