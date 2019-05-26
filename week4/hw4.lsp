;sat? receive a list of clauses and pass it to searching engine
;pass an empty list for model list, named mlist
;recieved n number of variables
(defun sat? (n delta) 
  (backTrack-searching n '() delta)
)


;helper function helps backtracking search to check the validity of clauses
;based on current mlist values
(defun helperFunc (mlist clauselist)
  (cond ((null clauselist) t)
	(t (and (deterClause mlist (car clauselist)) (helperFunc  mlist (cdr clauselist))))))



;back track search recursivly
;check wheter the clause is valid, if valid
;if the mlist already has all number we need, then return that list
;else perform a dfs and set 1 and -1 as true and false to the next unvisited num 
(defun backTrack-searching (n mlist clauses)
  (cond ((helperFunc mlist clauses) 
	 (cond ((= (length mlist) n) mlist) 
	       ((or (backTrack-searching n (addToList mlist 1) clauses) 
		    (backTrack-searching n (addToList mlist -1) clauses)))
	       ))))



;check the clause's validity with current mlist values
;check the clause's validity with unvisited value
(defun deterClause (mlist clause) 
  (cond ((null clause) nil)
	(t (cond ((deterClause-helpFunc mlist (car clause)) t)
		 (t (deterClause mlist (cdr clause)))))
))




;if element in the current mlist equal to the element passed in
;return true if not in the mlist
(defun deterClause-helpFunc (mlist element)
  (let* ((value (car mlist)))
    (cond ((null mlist) t)
	  ((= element value) t)
	  ((= (+ element value) 0) nil)
	  (t (deterClause-helpFunc (cdr mlist) element)))))


;generate the next value variable for the mlist
(defun generate-next(mlist)
  (+ (length mlist) 1)
)



;add a value to model list if not visited before
(defun addToList (mlist val)
  (let* ((value (generate-next mlist)))
    (cond ((> val 0) (append mlist (list value)))
	  (t (append mlist (list (- 0 value))))
	  )
    )
)