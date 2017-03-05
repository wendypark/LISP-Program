;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Your solution goes in this file ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Input Standards
;L = LIST
;A = ATOM
;NEL = NON EMPTY LIST
;N = NUMBER
;S = ARBITRARY S-EXPRESSION 
;F = FUNCTION


;count number of atoms that occur in all levels of list 
;returns number 
(DEFUN all-length (L)
  (COND 
    ;if, condition1 = list NULL, result = 0
    ((NULL L) 0)

    ;elif, condition2 = first element atom
    ((ATOM (CAR L))

      ;COND - nested
      (COND
        ;if, condition2.1 = first element NIL, result = 0, recur rest of list
        ((NULL (CAR L)) (+ 0 (all-length (CDR L)) ))

        ;else, result = +1, recur rest of list
        (T (+1 (all-length (CDR L))))
      )
    )

    ;else, condition3 = first element list, result = recur nested list + recur rest of top list
    (T (+ (all-length (CAR L)) (all-length (CDR L))))
  )
)



;list of length 2 of smallest and largest value 
;returns list
(DEFUN range (NEL)
  (COND
    ;if, condition1 = length of NEL is 1, result = new list (NEL NEL)
    ( (EQUAL (LENGTH NEL) 1) (LIST NEL NEL))

    ;else, condition2 = length of NEL > 1, result = new list (min (index x x+1) max (index x x+2))
    (T (LIST (MIN (CAR NEL) (CAR (range(CDR NEL)))) (MAX (CAR NEL) (CADR(range(CDR NEL)))) ))  
  )

)


;list of all elements between atoms/s-expression (inclusive)
;returns list 
(DEFUN before (A1 A2 L)
  (COND
    ;if, condition1 = list is empty, result = NIL
    ((NULL L) NIL)

    ;elif, condition2 = A1 is equal to first element of list
    ( (EQ A1 (CAR L))

      ;cond
      (COND

        ;if, condition2.1 = A2 is member of CDR L, result = cons A1 to second half of list (helper f1)
        ((MEMBER A2 (CDR L))    (CONS A1 (before_helper A2 (CDR L))))

        ;else, condtion2.2 = A2 is not a member of CDR L, result = list A1
        (T (LIST A1))
      )
    )

    ;else, condition3 = A1 not equal to first elemet of list, result = recurse rest of list 
    (T (before (A1 A2 (CDR L))))
  )

)


;list that goes until A2
;returns list
(DEFUN before_helper (A2 L)
  (COND
    ;if, condition1=first element of list = A2, result = list A2
    ((EQ A2 (CAR L)) (LIST A2))

    ;else, condition2 = first element of list != A2, result = recurse helper function 
    (T
      (CONS (before_helper A2 (CDR L)))
      )
  )
)


;list with 2 parts, members for which function F true, go into one list, and the rest go into other list
;returns list
(DEFUN split-if (F L) ;append 
  (COND
    ;if, condition1 = list is nill, result = new list (nil nil)
    ((NULL L) (LIST NIL NIL))

    ;else, result = (list (functioncall false) (functioncall true))
    (T (LIST((LIST (APPEND ())) (LIST))))

  )
)




(DEFUN split-if (F L)
    (COND
      ((NULL L) (LIST NIL NIL))
      (T (LIST (false_helper F L) (true_helper F L))
      );T
    );cond
);func

(DEFUN false_helper (F L)
  (DO 
    (F_L NIL (IF (NOT(FUNCALL F (CAR L))) (APPEND F_L (LIST(CAR L)))))
  )
  (RETURN F_L)
)

(DEFUN true_helper (F L)
  (DO 
    (T_L NIL (IF (FUNCALL F (CAR L)) (APPEND T_L (LIST(CAR L)))))
  )
  (RETURN T_L)
)









(defun split-if (f lst)
  (list (filter (lambda (x) (not (funcall f x))) lst) (filter f lst)))

; Support function for split-if. Returns a list containing all elements of lst for which function f returns truthy.
(defun filter (f lst)
  (cond
    ((null lst) nil) ; input is empty: return empty
    ((funcall f (car lst)) (cons (car lst) (filter f (cdr lst))))
    (t (filter f (cdr lst)))))



;new list with elements of L groups into sublists of length N, remainder put into sublist
;returns list
(DEFUN group (L N)
  (COND
    ;if, condition1 = list is NIL, result = nil
    ((NULL L) (LIST NIL))

    ;elif, condition2 = number is 1, result = new list(cdr list, N)
    ((EQUAL N 1) APPEND)

    ;



  )

)


;all elements for which the function yields highest score, along with itself, and value returned
;returns list
(DEFUN mostn (F L)
