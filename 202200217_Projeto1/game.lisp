;;;; game.lisp
;;;; Related to the domain of the game, operators, heuristics.
;;;; Author: Rodrigo Baptista 202200217

;;; Boards

(defun board-empty (&optional (rows 2) (columns 6))
  "Returns a 2x6 board with all cells empty."
  (make-list rows :initial-element (make-list columns :initial-element '0))
)

(defun board-test ()
  "Returns a 2x6 board that corresponds to exercise d)."
  '((1 2 3 4 5 6)
    (6 5 4 3 2 1))
)

(defun board-test1 ()
  "Returns a 2x6 board that corresponds to exercise d)."
  '((1 0 0 0 5 0)
    (6 0 4 0 0 0))
)

;;; Validators 

(defun valid-linep (index)
  "Returns T if INDEX is the index of a valid line in the board, NIL if it isn't."
  (or (= 0 index) (= 1 index))
)

(defun valid-line-indexp (index)
  "Returns T if INDEX is the index of a valid index of a line in the board, NIL if it isn't."
  (and (<= 0 index) (>= 5 index))
)

(defun line-emptyp (line)
  "Returns T if a line is empty (all cells have value 0)."
  (cond 
    ((null line) t)
    ((= (car line) 0) (line-emptyp (cdr line)))
    (t nil) 
  )
)

(defun board-emptyp (board)
  "Returns T if a board is empty (all cells have value 0)."
  (cond 
    ((null board) t)
    ((line-emptyp (car board)) (board-emptyp (cdr board)))
    (t nil)
  )
)

;;; Selectors 

(defun line (index board)
  "Returns the BOARD line present at INDEX."
  (if (valid-linep index)
    (nth index board)
    nil
  )
)

(defun cell (index1 index2 board)
  "Returns the value at the cell in the line at INDEX1, column INDEX2 of the BOARD."
  (if (and (valid-linep index1) (valid-line-indexp index2))
    (nth index2 (line index1 board))
    nil
  )
)

;;; Auxiliary Functions

(defun replace-position (position line &optional (value 0))
  "Replaces the value at POSITION in the LINE with VALUE."
  (cond
    ((null line) nil)
    ((= position 0) (cons value (replace-position (- position 1) (cdr line)))) ; We replace this value.
    (t (cons (car line) (replace-position (- position 1) (cdr line) value)))
  )
)

(defun replace-value (line-index position-index board &optional (value 0))
  "Replaces the value at LINE-INDEX and POSITION-INDEX of the BOARD with VALUE."
  (cond
    ((null board) nil)
    ((= line-index 0) (cons (replace-position position-index (car board) value) (cdr board))) ; The first line.
    (t (cons (car board) (list (replace-position position-index (car (cdr board)) value)))) ; The second line.
  )
)

(defun increment-value (line-index position-index board)
  "Increments the value at LINE-INDEX and POSITION-INDEX of the BOARD by 1."
  (let 
    ((cell-value (cell line-index position-index board)))
    (if (null cell-value)
      nil
      (replace-value line-index position-index board (+ cell-value 1))
    )
  )
)

;;; Operators

(defun distribute-pieces (number-of-pieces index1 index2)
  (labels 
    (
      (is-initial-hole (r c)
        "Returns true if R and C represent the initial hole, nil if it doesn't."
        (and (= r index1) (= c index2)))
      (go-through-dis-pie (r c np)
        "Returns the cells in which pieces will be distributed."
        (cond 
          ((= np 0) nil)
          ((and (= r 1) (= c 5) (is-initial-hole r c)) (go-through-dis-pie 0 c np)) ; Edge case bottom right and initial node.
          ((and (= r 0) (= c 0) (is-initial-hole r c)) (go-through-dis-pie 1 c np)) ; Edge case top left and initial node.
          ((and (= r 0) (is-initial-hole r c)) (go-through-dis-pie 0 (- c 1) np)) ; Go to the left.
          ((and (= r 1) (is-initial-hole r c)) (go-through-dis-pie 1 (+ c 1) np)) ; Go to the right.
          ((and (= r 1) (= c 5) (not (is-initial-hole r c))) (cons (list r c) (go-through-dis-pie 0 c (- np 1)))) ; Edge case bottom right.
          ((and (= r 0) (= c 0) (not (is-initial-hole r c))) (cons (list r c) (go-through-dis-pie 1 c (- np 1)))) ; Edge case top left.
          ((and (= r 0) (not (is-initial-hole r c))) (cons (list r c) (go-through-dis-pie 0 (- c 1) (- np 1)))) ; Go to the left.
          ((and (= r 1) (not (is-initial-hole r c))) (cons (list r c) (go-through-dis-pie 1 (+ c 1) (- np 1)))) ; Go to the right.
        )
      )
    )
    (if (and (valid-linep index1) (valid-line-indexp index2) (< 0 number-of-pieces))
      (go-through-dis-pie index1 index2 number-of-pieces)
      nil
    )
  )
)

(defun change-board (holes-list b)
  "Executes changes made to the board where HOLES-LIST are the holes to increment pieces and B the initial board."
  (if (null holes-list)
    b
    (let* 
      ( 
        (current-position (car holes-list))
        (line (nth 0 current-position))
        (column (nth 1 current-position))
        (next (cdr holes-list))
        (current-cell (cell line column b))
      )
      (cond 
        ((null holes-list) b) ; Return the changed board.
        ((and (null next) (or (= (+ current-cell 1) 1) (= (+ current-cell 1) 3) (= (+ current-cell 1) 5))) (replace-value line column b 0)) ; Final is 1, 3 or 5, to 0.
        (t (change-board (cdr holes-list) (increment-value line column b))) ; Go to the next hole with the new board.
      )
    )
  )
)

(defun game-operator (line-index position-index board)
  "Denotes the play is to be made at line INDEX1 and index INDEX2 of the BOARD."
  (if (and (valid-linep line-index) (valid-line-indexp position-index) (not (null board)))
    (let* 
      (
        (total-pieces (cell line-index position-index board))
        (holes (distribute-pieces total-pieces line-index position-index))
      )
      (if (= total-pieces 0)
        nil ; Invalid operation.
        (replace-value line-index position-index (change-board holes board) 0) ; Turns the moved value to zero and returns the new board.
      )
    )
    nil
  )
)

;;; Problem Domain Dependent Helper Functions

(defun board-piece-count (board)
  "Returns the total piece count of a given 2x6 BOARD."
  (loop for i1 below 2 
    sum (loop for i2 below 6 sum (cell i1 i2 board))
  )
)

(defun game-heuristic-base (board)
  "Receives a BOARD and returns the heuristic (h = o - c) where o is the number of pieces to capture and c is the number of pieces captured."
  (let* 
    (
      (total-pieces (get-total-pieces))
      (current-pieces (board-piece-count board))
      (captured-pieces (- total-pieces current-pieces)))
    (- total-pieces captured-pieces)
  )
)

(defun game-heuristic-advanced (board)
  "Heuristic based on total pieces left and their spread.
   Fewer pieces and clustered formations lead to a lower heuristic value."
  (let* ((total-pieces (board-piece-count board))
         (empty-spaces (count-empty-spaces board))
         (piece-spread (piece-spread-factor board)))
    (+ total-pieces (* 0.5 empty-spaces) (* 0.3 piece-spread))
  )
)

(defun count-empty-spaces (board)
  "Counts the number of empty spaces on the board."
  (loop for row in board sum (count 0 row))
)

(defun piece-spread-factor (board)
  "Calculates a spread factor based on piece distribution.
   A higher number indicates a more scattered board, which is worse."
  (let ((spread 0))
    (loop for i from 0 below 2 do
      (loop for j from 0 below 6 do
        (when (> (cell i j board) 0)
          (when (and (valid-linep i) (valid-line-indexp (- j 1))) (incf spread))
          (when (and (valid-linep i) (valid-line-indexp (+ j 1))) (incf spread))
          (when (and (valid-linep (- i 1)) (valid-line-indexp j)) (incf spread))
          (when (and (valid-linep (+ i 1)) (valid-line-indexp j)) (incf spread))
        )
      )
    )
    spread)
)


(defun node-solutionp (node)
  "Receives a NODE and checks if it's state is the problem solution."
  (if (or (null node) (null (node-state node)))
    nil
    (compare-state (create-node (board-empty) 0 0 nil) node) ; Creates a node with an empty board to check if the passed board has the same state.
  )
)

(defun compare-state (n1 n2)
  "Compares the state of N1 and N2 to check if two boards are identical."
  (let 
    (
      (n1-state (node-state n1))
      (n2-state (node-state n2))
    )
    (and (not (null n1-state)) (not (null n2-state)) (= (length n1-state) 2)
      (every #'(lambda (row1 row2) (every #'= row1 row2))  ; Rows are the same?
        n1-state n2-state)
    )
  )
)
