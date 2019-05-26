;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4);goal
(setq boxstar 5);box+goal, a box on top of goal
(setq keeperstar 6);keeper+goal, a keeper on top of goal

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
;(6)                                                                                                                                                                                                                   ;(setq p1 '((1 1 1 1 1 1)
;           (1 0 3 0 0 1)
;           (1 0 2 0 0 1)
;           (1 1 0 1 1 1)
;           (1 0 0 0 0 1)
;           (1 0 4 0 4 1)
;           (1 1 1 1 1 1))

(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row);x is column
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 


;*********************added Helper Function:
;to check if there is a box that not on top of goal
; check if the keeper is not on top of a goal
(defun existBoxOrKeeper(s_list)
  (cond ((null s_list) nil)
	((or (isKeeper (car s_list)) (isBox (car s_list)) t)
	 (t (existBox (cdr s_list)))
	)
	)
)


; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of the game.
; (neither any boxes nor the keeper is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
(defun goal-test (s)
  (cond ((null s) t)
	((existBoxOrKeeper (car s)) nil)
	(t (goal-test (cdr s)))
  );end cond
);end defun




; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;
;*******************below function computes the sucessor states of s*************
;this function will help to test whether keeper can move in each direction.
;keeper cannot move to the direction if:
;there is a wall
;or
;there is a box
;...and the box cannot move because (either exist another box next to the box on that direction,
;                   or exist a wall behind the box					

;******************below function helps to find the value at position (col, row)*********
;GETTER
(defun getValueOnPos(s col row)
  (car (nthcdr col (car (nthcdr row s))));this line gives the value on nth row and nth col of the row
)


;SETTER HELPER
(defun setHelper(s col newV)
  (cond ((null s) nil) 
	((= 0 col) (cons newV (cdr s)))
	(t (cons (car s) (setHelper (cdr s) (- col 1) newV))))
)

;SETTER
(defun setValueOnPos(s col row newV)
  (cond ((null s) nil)
	((= 0 row) (cons (setHelper (car s) col newV) (cdr s))) 
	(t(cons (car s) (setValueOnPos (cdr s) col (- row 1) newV)))
	);end cond
);end setValueOnPos


(defun try-move(s col row dir)
  (cond ((isWall (getValueOnPos s col row)) nil);can't move if is wall
	((or (isBox (getValueOnPos s col row)) 
	     (isBoxStar (getValueOnPos s col row)));if box
	 (cond ((equal dir '(up));moving up
		(cond ((or (isBoxStar (getValueOnPos s col (- row 1)))
			   (isBox (getValueOnPos s col (- row 1)))
			   (isWall (getValueOnPos s col (- row 1)))) nil);end cond1:if there is a box or  (*      )wall behind that box, can't move
		      
		      (t (cond ((isBoxStar (getValueOnPos s col row));this is the 1 step up of keeper's pos    (*box   )
			       (cond ((isKeeperStar (getValueOnPos s col (+ row 1)));this is the keeper's pos (*keeper)
				      (cond 
				       ((isStar (getValueOnPos s col (- row 1))) (setValueOnPos (setValueOnPos (setValueOnPos s col (- row 1) boxStar) col row keeperstar) col (+ 1 row) star));end cond2: if there is a star
				       ((isBlank (getValueOnPos s col (- row 1))) (setValueOnPos (setValueOnPos (setValueOnPos s col (- row 1) box) col row keeperstar) col (+ 1 row) star)))
				      )
				     
				     (t(cond 
					((isStar (getValueOnPos s col (- row 1))) (setValueOnPos (setValueOnPos (setValueOnPos s col (- row 1) boxStar) col row keeperstar) col (+ 1 row) blank))
					((isBlank (getValueOnPos s col (- row 1))) (setValueOnPos (setValueOnPos (setValueOnPos s col (- row 1) box) col row keeperstar) col (+ 1 row) blank)))
				       );end t
				     ))
			      
			      (t (cond ((isKeeperStar(getValueOnPos s col (+ row 1)))
					(cond
					 ((isStar (getValueOnPos s col (- row 1))) (setValueOnPos (setValueOnPos (setValueOnPos s col (- row 1) boxStar) col row keeper) col (+ 1 row) star))
					 ((isBlank (getValueOnPos s col (- row 1))) (setValueOnPos (setValueOnPos (setValueOnPos s col (- row 1) box) col row keeper) col (+ 1 row) star)))
					)
				       (t(cond
					  ((isStar (getValueOnPos s col (- row 1))) (setValueOnPos (setValueOnPos (setValueOnPos s col (- row 1) boxStar) col row keeper) col (+ 1 row) blank))
					  ((isBlank (getValueOnPos s col (- row 1))) (setValueOnPos (setValueOnPos (setValueOnPos s col (- row 1) box) col row keeper) col (+ 1 row) blank)))
					 )
				       ))
			      ))
		      );end cond or
		);end cond up
	       
	       ((equal dir '(down));moving down                                                                                                                   
		(cond ((or (isBox(getValueOnPos s col (+ row 1)))
			   (isBoxStar(getValueOnPos s col (+ row 1)))
                           (isWall (getValueOnPos s col (+ row 1)))) nil);end cond1:if there is a box or wall behind that box, can't move                                                                                                                    
		      (t (cond ((isBoxStar (getValueOnPos s col row));this is the 1 step below the keeper's pos    (*box   )
			       (cond ((isKeeperStar (getValueOnPos s col (- row 1)));this is the keeper's pos (*keeper)                                                                                                            
				      (cond
				       ((isStar (getValueOnPos s col (+ row 1))) (setValueOnPos (setValueOnPos (setValueOnPos s col (+ row 1) boxStar) col row keeperstar) col (- row 1) star));end cond2: if there is a star   
				       ((isBlank (getValueOnPos s col (+ row 1))) (setValueOnPos (setValueOnPos (setValueOnPos s col (+ row 1) box) col row keeperstar) col (- row 1) star)))
				      )
				     (t(cond
					((isStar (getValueOnPos s col (+ row 1))) (setValueOnPos (setValueOnPos (setValueOnPos s col (+ row 1) boxStar) col row keeperstar) col (- row 1) blank))
					((isBlank (getValueOnPos s col (+ row 1))) (setValueOnPos (setValueOnPos (setValueOnPos s col (+ row 1) box) col row keeperstar) col (- row 1) blank)))
				       );end t                                                                                                                                                                                 
				     ))
			      
			      (t (cond ((isKeeperStar(getValueOnPos s col (- row 1)));else if 1 step below the keeper is a box
					(cond
					 ((isStar (getValueOnPos s col (+ row 1))) (setValueOnPos (setValueOnPos (setValueOnPos s col (+ row 1) boxStar) col row keeper) col (- row 1) star))
					 ((isBlank (getValueOnPos s col (+ row 1))) (setValueOnPos (setValueOnPos (setValueOnPos s col (+ row 1) box) col row keeper) col (- row 1) star)))
					)
				       (t(cond
					  ((isStar (getValueOnPos s col (+ row 1))) (setValueOnPos (setValueOnPos (setValueOnPos s col (+ row 1) boxStar) col row keeper) col (- row 1) blank))
					  ((isBlank (getValueOnPos s col (+ row 1))) (setValueOnPos (setValueOnPos (setValueOnPos s col (+ row 1) box) col row keeper) col (- row 1) blank)))
					 )
				       ));end t
			      );end cond
			);end t
		      );end cond
		);end down
	 
	      
	       ((equal dir '(left));moving left                                                                                                                                                                                    
                 (cond ((or (isBox(getValueOnPos s (- col 1) row))
                            (isBoxStar(getValueOnPos s (- col 1) row))
                            (isWall (getValueOnPos s (- col 1) row)))  nil);end cond1:if there is a box or wall behind that box, can't move                                                                                         
		       (t(cond ((isBoxStar (getValueOnPos s col row));this is the 1 step right to the keeper's pos    (*box   )                                                                                                     
                                (cond ((isKeeperStar (getValueOnPos s (+ col 1) row));this is the keeper's pos (*keeper)                                                                                                                                                                                                                                                                                                                                       
				       (cond
                                       ((isStar (getValueOnPos s (- col 1) row)) (setValueOnPos (setValueOnPos (setValueOnPos s (- col 1) row boxStar) col row keeperstar) (+ col 1) row star));end cond2: if there is a star 
				       ((isBlank (getValueOnPos s (- col 1) row)) (setValueOnPos (setValueOnPos (setValueOnPos s (- col 1) row box) col row keeperstar) (+ col 1) row star)))
				       );end isKeeperStar 
				      (t(cond;else if isKeeper                                                                                                                                                                       
					 ((isStar (getValueOnPos s (- col 1) row)) (setValueOnPos (setValueOnPos (setValueOnPos s (- col 1) row boxStar) col row keeperstar) (+ col 1) row blank))
                                         ((isBlank (getValueOnPos s (- col 1) row)) (setValueOnPos (setValueOnPos (setValueOnPos s (- col 1) row box) col row keeperstar) (+ col 1) row blank)))
                                        );end t
				      );end cond                                                                                                                                                                                     
				);end isboxstar                                                                                                                                                                                      
                               (t (cond ((isKeeperStar(getValueOnPos s (+ col 1) row));else if 1 step right the keeper is a box                                                                                                
					 (cond
                                          ((isStar (getValueOnPos s (- col 1) row));(keeperstar box )                                                                                                                                 
                                           (setValueOnPos (setValueOnPos (setValueOnPos s (- col 1) row boxStar) col row keeper) (+ col 1) row star))
                                          ((isBlank (getValueOnPos s (- col 1) row));(blank box keeperstar)                                                                                                                          
                                           (setValueOnPos (setValueOnPos (setValueOnPos s (- col 1) row box) col row keeper) (+ col 1) row star))
                                          );end cond                                                                                                                                                                                  
                                         );end iskeeperstar                                                                                                                                                                           
                                        (t(cond;else if is keeper                                                                                                                                                                     
                                           ((isStar (getValueOnPos s (- col 1) row))
                                            (setValueOnPos (setValueOnPos (setValueOnPos s (- col 1) row boxStar) col row keeper) (+ col 1) row blank))
                                           ((isBlank (getValueOnPos s (- col 1) row))
                                            (setValueOnPos (setValueOnPos (setValueOnPos s (- col 1) row box) col row keeper) (+ col 1) row blank)))
                                          );end t                                                                                                                                                                                     
                                        );end cond                                                                                                                                                                                    
                                  );end t                                                                                                                                                                                             
			       );end cond                                                                                                                                                                                           \
                                                                                                                                                                                                                                      
                         );end t                                                                                                                                                                                                      
                       );end cond 
		 )
	      
		((equal dir '(right));moving left                                                                                                                                                                                    
		 (cond ((or (isBox(getValueOnPos s (+ col 1) row))
			    (isBoxStar(getValueOnPos s (+ col 1) row))
			    (isWall (getValueOnPos s (+ col 1) row)))  nil);end cond1:if there is a box or wall behind that box, can't move                                                                                 
		  
		       (t(cond ((isBoxStar (getValueOnPos s col row));this is the 1 step right to the keeper's pos    (*box   )                                                                                                         
				(cond ((isKeeperStar (getValueOnPos s (- col 1) row));this is the keeper's pos (*keeper)                                                                                                                
				       (cond
					((isStar (getValueOnPos s (+ col 1) row)) 
					 (setValueOnPos (setValueOnPos (setValueOnPos s (+ col 1) row boxStar) col row keeperstar) (- col 1) row star));end cond2: if there is a star         
					((isBlank (getValueOnPos s (+ col 1) row)) 
					 (setValueOnPos (setValueOnPos (setValueOnPos s (+ col 1) row box) col row keeperstar) (- col 1) row star)))
				       );end isKeeperStar                                                                                                                                                                                 
				      (t(cond;else if isKeeper                                                                                                                                                                        
					 ((isStar (getValueOnPos s (+ col 1) row))
					  (setValueOnPos (setValueOnPos (setValueOnPos s (+ col 1) row boxStar) col row keeperstar) (- col 1) row blank))
					 ((isBlank (getValueOnPos s (+ col 1) row)) 
					  (setValueOnPos (setValueOnPos (setValueOnPos s (+ col 1) row box) col row keeperstar) (- col 1) row blank)))
					);end t                                                                                                                                                                                        
				      );end cond                                                                                                                                                                                        
				);end isboxstar
			       
			       (t (cond ((isKeeperStar(getValueOnPos s (- col 1) row));else if 1 step right the keeper is a box                                                                                                       
					 (cond
					  ((isStar (getValueOnPos s (+ col 1) row));(keeperstar box )                                                                                                                               
					   (setValueOnPos (setValueOnPos (setValueOnPos s (+ col 1) row boxStar) col row keeper) (- col 1) row star))					  
					  ((isBlank (getValueOnPos s (+ col 1) row));(blank box keeperstar)                                                                                                                          
					   (setValueOnPos (setValueOnPos (setValueOnPos s (+ col 1) row box) col row keeper) (- col 1) row star))
					  );end cond
					 );end iskeeperstar
					
					(t(cond;else if is keeper                                                                                                                                                               
					   ((isStar (getValueOnPos s (+ col 1) row))
					    (setValueOnPos (setValueOnPos (setValueOnPos s (+ col 1) row boxStar) col row keeper) (- col 1) row blank))
					   ((isBlank (getValueOnPos s (+ col 1) row))
					    (setValueOnPos (setValueOnPos (setValueOnPos s (+ col 1) row box) col row keeper) (- col 1) row blank)))
					  );end t
					);end cond
				  );end t
				);end cond                                                                                                                                                                                            
			 );end t
		       );end cond                                                                                                                                                                                                      
		 );end cond right
		));end isBox

	((isBlank (getValueOnPos s col row))
	 (cond ((equal dir '(up)) 
		(cond ((isKeeperStar(getValueOnPos s col (+ row 1)))
		       (setValueOnPos (setValueOnPos s col row keeper) col (+ 1 row) star))
		      (t(setValueOnPos (setValueOnPos s col row keeper) col (+ 1 row) blank))))
	       ((equal dir '(down)) 
		(cond ((isKeeperStar(getValueOnPos s col (- row 1)))
		       (setValueOnPos (setValueOnPos s col row keeper) col (- row 1) star ))
		      (t(setValueOnPos (setValueOnPos s col row keeper) col (- row 1) blank ))))
	       ((equal dir '(left)) 
		(cond ((isKeeperStar(getValueOnPos s (+ col 1) row))
		       (setValueOnPos (setValueOnPos s col row keeper) (+ col 1) row star ))
		      (t(setValueOnPos (setValueOnPos s col row keeper) (+ col 1) row blank ))))
               ((equal dir '(right))
		(cond ((isKeeperStar(getValueOnPos s (- col 1) row))
		       (setValueOnPos (setValueOnPos s col row keeper) (- col 1) row star ))
		      (t(setValueOnPos (setValueOnPos s col row keeper) (- col 1) row blank))))
	       );end cond
	 );end isBlank

	((isStar (getValueOnPos s col row))
	 (cond ((equal dir '(up))
                (cond ((isKeeperStar(getValueOnPos s col (+ row 1)))
                       (setValueOnPos (setValueOnPos s col row keeperstar) col (+ 1 row) star))
                      (t(setValueOnPos (setValueOnPos s col row keeperstar) col (+ 1 row) blank))));end up
               ((equal dir '(down))
                (cond ((isKeeperStar(getValueOnPos s col (- row 1)))
                       (setValueOnPos (setValueOnPos s col row keeperstar) col (- row 1) star ))
                      (t(setValueOnPos (setValueOnPos s col row keeperstar) col (- row 1) blank ))));end down
               ((equal dir '(left))
                (cond ((isKeeperStar(getValueOnPos s (+ col 1) row))
                       (setValueOnPos (setValueOnPos s col row keeperstar) (+ col 1) row star ))
                      (t(setValueOnPos (setValueOnPos s col row keeperstar) (+ col 1) row blank ))));end left
               ((equal dir '(right))
                (cond ((isKeeperStar(getValueOnPos s (- col 1) row))
                       (setValueOnPos (setValueOnPos s col row keeperstar) (- col 1) row star ))
                      (t(setValueOnPos (setValueOnPos s col row keeperstar) (- col 1) row blank))));end right
               );end cond   	       
	 );end isStar
	);end cond
 );end defun
;***********************		       
		      
				       

(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0));pos will be (x y)
	 (x (car pos));x is col
	 (y (cadr pos));y is row
	 ;x and y are now the coordinate of the keeper in s.
	 (result (list (try-move s x (- y 1) '(up)) ;up,    row-1
		       (try-move s x (+ 1 y) '(down)) ;down,  row+1
		       (try-move s (- x 1) y '(left)) ;left,  col-1
		       (try-move s (+ 1 x) y '(right)) ;right, col+1
	 )))
	 (cleanUpList result);end
   );end let
  );

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
  0
)

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;
;
(defun h1-helper(s)
  (cond ((null s) 0)
	((isBox (car s)) (+ 1 (h1-helper (cdr s))))
	(t (h1-helper (cdr s)) )
	)
)

(defun h1 (s)
  (cond ((null s) 0)
	(t(+ (h1-helper (car s)) (h1 (cdr s)))))
)

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;
;(defun h<804907607> (s)


 ; )
(defun h804258351 (s)
  (let* ((pos (getKeeperPosition s 0))
	  (c (car pos))
	   (r (cadr pos)))

     (*
       (get-move-cost 
	(append (list r) (list c))
	(get-box-coords S 0)
	 )
        (h1 s)
	 )


  )
)

;We need to sum the Manhattan distances from player to each box
; player position is list pos = (r c)
(defun get-move-cost (pos box_coords )
  (cond
   ((null box_coords) 0)
   (t
    (+
     (calc-manhattan pos (first box_coords))
     (get-move-cost pos (rest box_coords))
     )
    )

   )
)

;Calculate Manhattan Distance between two points (x1, y1) (x2, y2)
(defun calc-manhattan (P1 P2)
  (+ 
   (abs (- (first P1) (first P2)))
   (abs (- (second P1) (second P2)))
   )
)

;Searches a row for boxes, returns list of col positions L_c
(defun search-row (R i)
  (cond
   ((null R) NIL)
   ((isBox (first R)) (append (list i) (search-row (rest R) (+ i 1)) ))
   (t
    (search-row (rest R) (+ i 1))
    )
   )
)

;Create coordinate list 
;Example: R_num = 0, C_boxvals = (1 2 3 4)
;Output: ((0 1) (0 2) (0 3) (0 4))

(defun create-pairs (R_num C_boxvals)
  (cond
   ((null C_boxvals) NIL)
   (t
    (append
     (list(append (list R_num) (list(first C_boxvals))))
     (create-pairs R_num (rest C_boxvals))
     )
    )
   )

)


;Get all of the coordinates where each box is located.
(defun get-box-coords (S i)
  (cond
   ((null S) NIL)

   ;row 0 from this perspective
   (t
    (append
     (create-pairs i (search-row (first S) 0))
     (get-box-coords (rest S) (+ i 1))
     )
    )
   )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also provide a number which indicates the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below has optimal solution depth 6.
 | As for the solution depth, any admissible heuristic must make A* return an optimal solution. So, the depths of the optimal solutions provided could be used for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#
;(6)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))

;(15)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 4 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(13)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 4 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(17)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 4 0)
	   (0 3 1 0 0 0 0)))

;(12)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 4 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(13)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 4 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(47)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 4 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 4 0 4 1)
	   (1 1 1 1 1 1)))

;(34)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 4 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(59)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 4 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(?)
(setq p11 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0  0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 4 0)
	    (0 0 1 4 0 0 0)))

;(?)
(setq p12 '((1 1 1 1 1 0 0 0)
	    (1 0 0 4 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(?)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 4 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(?)
(setq p14 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))

;(?)
(setq p15 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 4 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
