;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Your solution goes in this file ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun all-length (top_list)
 	"counts the number of elements in top level of a list"
 	(length top_list)
) 

(defun range (list_num)
 	"takes a list of numbers with at least one element and returns list of length 2 of smallest and largest numbers"
 	(list (apply 'min list_num) (apply 'max list_num))
)