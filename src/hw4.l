;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Your solution goes in this file ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    ((EQ (LENGTH list_num) 1)
      (LIST (CAR list_num) (CAR list_num))
      )
    (T
      (LIST
        (min (CAR list_num) (CAR(range (CDR list_num))) )
        (max (CAR list_num) (CAR(CDR(range (CDR list_num)))) )
      )
    ) 
  )
)


; Go forward until begin
  ; Signify begin by set begin to NIL
  ; Once begin is found, go forward until end

; Working
(defun before (begin end list_input)
	(COND
    ; Return empty list
		((NULL list_input) NIL)

    ; Case: first == to begin OR begin is NIL (Signifying begin was found)
    (
      (OR
        (EQUAL (CAR list_input) begin)
        (NULL begin)
        )

      (COND
        ; if equals to end, end recursion
        ((EQUAL (CAR list_input) end)
          (LIST end)
          )
        (T
          (CONS (CAR list_input) (before NIL end (CDR list_input)))
          )
        )
      )

    ; Skip forward
    (T
      (before begin end (CDR list_input))
      )
	)
)

;; Not true is working, but true is not working...
(defun split-if (f list_input)
  (COND
    ;; Return two empty list_inputs
    ((NULL list_input)
      (LIST NIL NIL)
      )
    ;; Return split-if[0], split-if[1].append(list_input.first)
    ((funcall f (CAR list_input))

      (let ((result (split-if f (CDR list_input))))
        (LIST
          (CAR result)
          (CONS
            (CAR list_input)
            (CAR (CDR result))
            )
          )
        )

      )
    ;; Return split-if[0].append(list_input.first), split-if[1]
    (T 
      (let ((result (split-if f (CDR list_input))))
        (LIST
          (CONS
            (CAR list_input)
            (CAR result)
            )
          (CAR (CDR result))
          )
        )
      )
    )
  )
;; NOTE: Passing functions as parameters: http://emacs.stackexchange.com/questions/14747/how-to-pass-function-as-argument-in-elisp


; Going from backwards
(defun group_helper (reversed_list_input n)
  (COND
    ((NULL reversed_list_input)
      (LIST NIL)
      )
    (T
      (let ((result (group_helper (CDR reversed_list_input) n)))
        ; Add to result[0] if result[0].length < n
        (COND
          ((< (LENGTH (CAR result)) n)
            (LIST
              (CONS (CAR reversed_list_input)  (CAR result))
              (CDR result)
              )
            )
          ; Else create a new result[0]
          (T
            (LIST
              (LIST (CAR reversed_list_input))
              result
              ) 
            );T
          );COND
        );let
      );T
    );COND
  )

; If null, return nil
; 

(defun group (list_input n)
  (COND 
    ((NULL list_input) nil )
    (T
      (REVERSE (group_helper (REVERSE list_input) n))
      )
    );COND
  );defun
