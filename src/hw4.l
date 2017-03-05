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


; Go forward until nil
  ; If result.last == NIL OR result.last.length >= n
    ; add new list to beginning of result
  ; ELSE
    ; append value to result.last

; Go backwards from front until nil
  ; If result.last == NIL OR result.last.length >= n
    ; append new list to result
    ; where new list = (list_input.last)
  ; ELSE
    ; append list_input.last to result.last

(defun group (list_input n)
  (COND
    ((NULL list_input)
      NIL
      );(NULL
    (T
      ; Recurse on all but last element of list
      (let ((result (group (BUTLAST list_input) n)))
        (COND
          ; If result.last == NIL OR result.last.length >= n
          ((NULL result)
            ; (( input ))
            (LIST (LIST (CAR (last list_input))))
            );(NULL

          ((>= (LENGTH (CAR (last result))) n)
            (APPEND
              ; (( ...result )) += (( input ))
              result
              (LIST (LIST (CAR (last list_input))))
              );APPEND
            );(OR
          (T
            (APPEND
              ;; (( ...result )) += ((...result + input))
              (BUTLAST result)
              (LIST
                (APPEND
                  ;; ( ...result )
                  (CAR (last result))
                  ;; ( input )
                  (LIST (CAR (last list_input)))
                  );APPEND
                );LISt
              );APPEND
            );T
          );COND
        );let
      );T
    );COND
  );defun

(defun mostn (func list_input)
  T
  )

(defun match (pattern assertion)
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
(defun match_helper (pattern assertion)
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



