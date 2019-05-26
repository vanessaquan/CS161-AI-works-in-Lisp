;#1 this is a small lisp program that returns a single top level list of the terminal nodes that 
;visitied by a right-to-left depth-first search
;the logic of this program is to check the base case
;(1) when reach to the end of list, return nil
;(2) when (car list) is an atom, simply append this atom to end of list
;(3) when (car list) is not an atom, append (car list) (rest list) to subtract out the bracket
(defun DFS( LIST)
  (cond ((NULL LIST) nil) 
	((listp (car LIST)) (DFS (append (car LIST) (rest LIST))))
	((atom (car LIST))  (append (DFS (rest LIST)) (list (car LIST))))
	))


	
;#2 depth-frist iterative-deepening
;function DFID-depth(L currLevel):
;   this function basically do what "limited-depth search " do, instead, it also output the 
;   top-level nodes with the given depth
(defun DFID-depth(L currLevel)
  (cond ((NULL L) nil)
	((atom L) (cons L nil))
	((= currLevel 0) nil)
	(t(append (DFID-depth (rest L) currLevel) (DFID-depth (first L) (- currLevel 1))))
)) 

;#2 this function will recurssively append the ouput list of each depth of the tree
(defun DFID(L currLevel)
  (cond ((= 0 currLevel) nil) 
	(t (append (DFID L (- currLevel 1)) (DFID-depth L currLevel)))
))




; These functions implement a depth-first solver for the missionary-cannibal
; problem. In this problem, three missionaries and three cannibals are trying to
; go from the east side of a river to the west side. They have a single boat
; that can carry two people at a time from one side of the river to the
; other. There must be at least one person in the boat to cross the river. There
; can never be more cannibals on one side of the river than missionaries. If
; there are, the cannibals eat the missionaries.

; In this implementation, a state is represented by a single list
; (missionaries cannibals side). side represents which side the boat is
; currently on, and is T if it is on the east side and NIL if on the west
; side. missionaries and cannibals represent the number of missionaries and
; cannibals on the same side as the boat. Thus, the initial state for this
; problem is (3 3 T) (three missionaries, three cannibals, and the boat are all
; on the east side of the river) and the goal state is (3 3 NIL).

; The main entry point for this solver is the function MC-DFS, which is called
; with the initial state to search from and the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, MC-DFS returns NIL.

; To call MC-DFS to solve the original problem, one would call (MC-DFS '(3 3 T)
; NIL) -- however, it would be possible to call MC-DFS with a different initial
; state or with an initial path.

; Examples of calls to some of the helper functions can be found after the code.



; FINAL-STATE takes a single argument s, the current state, and returns T if it
; is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
  (cond ((equal s '(3 3 nil)) t)
	(t NIL)
  )
)

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (s), a number of
; missionaries to move (m), and a number of cannibals to move (c). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).

;cases for this function:
;1. check whether the move of m and c result in negative states.
;2. predict whether move the number of m or c will result an invalid movement
;3. check both side (east and west/ nil and t) return nil if any of these two sides 
;   has missioners < cannibles.
;4. otherwise, which means none of the above invalid movement occurs, 
;   return back the states after movement. 
;*********things I'm not clear:  can the sum of m+c > 2?********
(defun next-state (s m c)
  (cond ((> (+ m c) 2) nil)
	((< (- (first s) m) 0) nil)
	((< (- (second s) c) 0) nil)
	((and (< (- (first s) m) (- (second s) c)) (not (= (- (first s) m) 0))) nil)
	((and (< (+ (- 3 (first s)) m) (+ (- 3 (second s)) c)) (not (= (- 3 (first s)) 0))) nil)
	(t(list (append (list (+ (- 3 (first s)) m)) (list (+ (- 3 (second s)) c)) (list (not (third s))))))
	))

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun succ-fn (s)
  (append (next-state s 0 1) (next-state s 1 0) (next-state s 1 1) (next-state s 0 2) (next-state s 2 0))
)

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (s) and the
; stack of states visited by MC-DFS (states). It returns T if s is a member of
; states and NIL otherwise.
(defun on-path (s states)
  (cond ((null states) nil)
	(t(or (equal (car states) s) (on-path s (cdr states))))
  )
)

; MULT-DFS is a helper function for MC-DFS. It takes two arguments: a stack of
; states from the initial state to the current state (path), and the legal
; successor states to the last state on that stack (states). states is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of states in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
(defun mult-dfs (states path)
  (cond ((null states) nil)
	((mc-dfs (first states) path) (mc-dfs (first states) path))
	(t(mult-dfs (rest states) path)))
)

; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. MC-DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun mc-dfs (s path)
  (cond ((final-state s) (append path (list s)))
	((not (on-path s path)) (mult-dfs (succ-fn s) (append path (list s))))
	(t nil)
))



; Function execution examples

; Applying this operator would result in an invalid state, with more cannibals
; than missionaries on the east side of the river.
; (next-state '(3 3 t) 1 0) -> NIL

; Applying this operator would result in one cannibal and zero missionaries on
; the west side of the river, which is a legal operator. (NOTE that next-state
; returns a LIST of successor states, even when there is only one successor)
; (next-state '(3 3 t)