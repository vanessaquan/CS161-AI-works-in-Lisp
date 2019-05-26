;#1
;n:the number needs to check on tree/ TREE: an ordered tree
;if the TREE is empty, number not exist and return NIL
;if TREE is not a list/ is a single number
;check if the number is target and return true if found
;else recursively call the function and pass in the rest of TREE
;if (car TREE) is a list, use appended value as input to remove an outer braket 
(defun TREE-CONTAINS( n TREE)
  (cond ( (NULL TREE) NIL)
        ((equal (listp (car TREE)) NIL)
	 (cond ((= (car TREE) n) t)
	       (t(TREE-CONTAINS n (cdr TREE)))
               )
        )
        (t(TREE-CONTAINS n (append (car TREE) (cdr TREE))))
  )
)

;#2
;return NIL if TREE is Empty
;always check the car of TREE, if car Tree is not a list, return that number directly
; use the append result as input so that the first layer of braket is removed
(defun TREE-MIN(TREE)
  (cond ((NULL TREE) NIL)
	( (equal(listp (car TREE)) NIL) (car TREE) )
	(t(TREE-MIN (car TREE)))
  )
)


;#3
;return NIL if tree is empty
;if Tree is a simple number, add that number to result
;if the input is a list
;if the list has three elements inside
;first add the middle(root) number to result
; recursively call the function and go through the leftside of the tree 
;recursively call the function and go through the rightside of the tree
;if the list contain only one element inside
;add that element to the result
(defun TREE-ORDER(TREE)
 (cond ((NULL TREE) NIL)
       ((numberp TREE) (cons TREE NIL))
       ((listp TREE)
	(cond ( (= (length TREE) 3)
		(cons (second TREE)
		      (append (TREE-ORDER (first TREE))
			      (TREE-ORDER (third TREE)))))
	      ( (= (length TREE) 1)
		(cons (car TREE) NIL))))))


;#4
;input of a LIST , start position and length
;return NIL if list is empty
;return NIL if l is less than list length
;reduce position by 1 and return the rest of list 
;if pos is 0, append the first element from list and return the rest until l reach to 0
(defun SUB-LIST(LIST pos l)
 (cond ((NULL LIST) NIL)
       ((> l (length LIST)) NIL)
       ((> pos 0) 
	(cond      ((> l (length LIST)) NIL)
		   (t(SUB-LIST (cdr LIST) (- pos 1) l))))
       ((= pos 0) (cond ((> l 0) (append (list (car LIST)) (append (SUB-LIST (cdr LIST) pos (- l 1)) NIL)))))
))


;#5
;Split the input list into equal two half if the input length is an even number
;split the input list into half so that the first half is 1 unit greater than the second half
(defun SPLIT-LIST(LIST)
  (cond ((evenp (length LIST)) (append (list (SUB-LIST LIST 0 ( / (length LIST) 2)))
				       (list (SUB-LIST LIST ( / (length LIST) 2) (/ (length LIST) 2)))))
	((oddp (length LIST)) (append (list (SUB-LIST LIST 0 (/ (+ (length LIST) 1) 2)))
				      (list (SUB-LIST LIST (/(+ (length LIST) 1) 2) (/(- (length LIST) 1) 2)))))
))



;#6
;recursively check depth of the left half and the depth of the right half
;let the result returned by the left half stored in "left"
;result of right half stored in "right"
;base cases:
;BTREE is a simple atom, return 0
;BTREE is a noun nested list with two element, return 1
;otherwise, recursively add 1 and return the bigger result. 
(defun BTREE-HEIGHT(BTREE)
  (cond
	((atom BTREE) 0)
	((listp BTREE)
	 (cond ((and (atom (car BTREE)) (atom (cadr BTREE))) 1)
	       ((= (length BTREE) 1) (+ 1 (BTREE-HEIGHT(car BTREE))))
	       ((= (length BTREE) 2)
		(let ((left (+ 1 (BTREE-HEIGHT(car BTREE)))) (right (+ 1 (BTREE-HEIGHT(cadr BTREE)))))
		   (cond ((> right left) right)
			 (t left)
)))))))




;#7
;The input value is a list
;list may have a length of 1 2 or more than 2
;cond 1: if length of list is just 1, return the atom on the list
;cond 2: if length of list is exactly 2, return the pair (atom1, atom2)
;cond 3: if length of list is more than 2, recursively split the list and its nested list which length is > 2
(defun LIST2BTREE(LIST)
  (cond (( NULL LIST) NIL)
	((listp LIST) 
	 (cond ((= (length LIST) 2) LIST) 
	       ((= (length LIST) 1) (car LIST))
	       ((> (length LIST) 2) (cons (LIST2BTREE (first  (SPLIT-LIST LIST)))
					  (cons (LIST2BTREE (second (SPLIT-LIST LIST))) NIL)))
))))



;#8
;recursively append the car of TREE and return

(defun BTREE2LIST(TREE)
  (cond ( (NULL TREE) NIL)
	((atom TREE) TREE)
        ((atom (car TREE))
	  (append (list(car TREE)) (BTREE2LIST (cdr TREE))))
	(t(BTREE2LIST (append (car TREE) (cdr TREE))))
  )
)


;#9
;compare whether E1 and E2 has the exactly same format and number values
;return NIL if any parenthesis and numbers does not match 
(defun IS-SAME( E1 E2)
  (cond ((and (NULL E1) (NULL E2)) T)
	((and (numberp E1) (numberp E2))
	 (cond ((= E1 E2) T)
	       ((not (= E1 E2)) NIL)))
	((and (listp E1) (listp E2)) (and (IS-SAME (car E1f) (car E2)) (IS-SAME (rest E1) (rest E2))))
))  



;below are some of the test case
;(TREE-CONTAINS 3 '((1 2 3) 7 8))
;(TREE-MIN '((1 2 3) 7 8))
;(TREE-ORDER 3)
;(SUB-LIST '(a b c d) 0 3)
;(SPLIT-LIST '(a b c d))
;(BTREE-HEIGHT '(((1 2) (3 4)) ((5 6) (7 8))))
;(LIST2BTREE '(1 2 3 4 5 6 7 8))
;(BTREE2LIST '(((1 2) (3 4)) ((5 6) (7 8))))
;(IS-SAME '((1 2 3) 7 8) '((1 2 3) 7 8)) 

