;;;; puzzle.lisp
;;;; Related to the domain of the game, operators, heuristics.
;;;; Author: Rodrigo Baptista 202200217

;;; Boards

(defun board-empty (&optional (rows 2) (columns 6))
  "Returns a 2x6 board with all cells empty."
  (make-list rows :initial-element (make-list columns :initial-element '0))
)

; TODO: Make sure this is initial.
(defun board-initial ()
  "Returns a 2x6 board that corresponds initial game state."
  '((8 8 8 8 8 8)
    (8 8 8 8 8 8))
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
          ((and (= r 0) (is-initial-hole r c)) (go-through-dis-pie 0 (- c 1) np)) ; Go to the left and initial node.
          ((and (= r 1) (is-initial-hole r c)) (go-through-dis-pie 1 (+ c 1) np)) ; Go to the right and initial node.
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

(defun change-board (holes-list node initial-line)
  "Executes changes made to the board where HOLES-LIST are the holes to increment pieces and NODE contains the state of the initial board. INITIAL-LINE denotes the current player."
  (if (null holes-list)
    node
    (let* 
      ( 
        (current-position (car holes-list))
        (line (nth 0 current-position))
        (column (nth 1 current-position))
        (next (cdr holes-list))
        (current-cell (cell line column (node-state node)))
      )
      (cond 
        ((null holes-list) node) ; Return the resulting node.
        ((and (null next) (or (= (+ current-cell 1) 1) (= (+ current-cell 1) 3) (= (+ current-cell 1) 5)) (not (= line initial-line))) ; Final is 1, 3 or 5, to 0, if the line is not the starting line.
          (if (= initial-line 1) ; Player one is always the SECOND line (INDEX 1) of the board. While player two is always the FIRST line (INDEX 0) of the board.
            (create-node 
              (replace-value line column (node-state node) 0) 
              (+ (node-score-p1 node) (+ current-cell 1)) 
              (node-score-p2 node)
              0 ; Doesn't matter here.
              0 ; Doesn't matter here.
            ) ; Returns the new node. Player 1 captures.
            (create-node 
              (replace-value line column (node-state node) 0) 
              (node-score-p1 node) 
              (+ (node-score-p2 node) (+ current-cell 1)) 
              0 ; Doesn't matter here.
              0 ; Doesn't matter here.
            ) ; Returns the new node. Player 2 captures.
          ) 
        ) 
        (t 
          (change-board 
            (cdr holes-list) 
            (create-node 
              (increment-value line column (node-state node)) 
              (node-score-p1 node) 
              (node-score-p2 node)
              0 ; Doesn't matter here.
              0 ; Doesn't matter here.
            ) ; New node where no captures were made.
            initial-line)) ; Go to the next hole with the new node.
      )
    )
  )
)

(defun game-operator (line-index position-index node)
  "Denotes the play is to be made at line INDEX1 and index INDEX2 of the board in NODE."
  (if (and (valid-linep line-index) (valid-line-indexp position-index) (not (null node)))
    (let* 
      (
        (total-pieces (cell line-index position-index (node-state node)))
        (holes (distribute-pieces total-pieces line-index position-index))
      )
      (if (= total-pieces 0)
        nil ; Invalid operation.
        (let 
          ((new-node (change-board holes node line-index))) ; The new node, with updated state & scores.
          (create-node 
            (replace-value line-index position-index (node-state new-node) 0) 
            (node-score-p1 new-node) 
            (node-score-p2 new-node)
            (+ (node-depth node) 1)
            node
          ) ; Turns the moved value to zero and returns the new node.
        )
      )
    )
    nil
  )
)

;;; Problem Domain Dependent Helper Functions

(defun new-child (node operator index1 index2)
  "Generates the child of a given NODE according to OPERATOR, called at INDEX1 and INDEX2."
  (when node
    (let 
      ((child (funcall operator index1 index2 node))) ; Calls the operator, which returns a node with the new board and score.
      (if (not (null child))
        child
        nil
      )
    )
  )
)  

(defun generate-children (node operator current-player)
  "Generates the children of a given NODE using OPERATOR according to CURRENT-PLAYER."
  (when 
    (and (not (null node)))
    (let 
      (
        (children '())
        (i1 (if (= current-player 1) 1 0))
      )
      (loop for i2 from 0 below 6 do ; Board columns.
        (let 
          ((child (new-child node operator i1 i2)))
          (when child (push child children))
        )
      )
      (nreverse children)
    )
  )
)

(defun board-piece-count (board)
  "Returns the total piece count of a given 2x6 BOARD."
  (loop for i1 below 2 
    sum (loop for i2 below 6 sum (cell i1 i2 board))
  )
)

(defun line-piece-count (line-index board)
  "Returns the total piece count of a given line of a BOARD."
  (reduce #'+ (nth line-index board))
)

(defun node-solutionp (node)
  "Receives a NODE and checks if it's state is the problem solution."
  (if (or (null node) (null (node-state node)))
    nil
    (compare-state (create-node (board-empty) 0 0 0 nil) node) ; Creates a node with an empty board to check if the passed board has the same state.
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

(defun evaluate-node (node)
  "Evaluates the score of a given NODE."
  (- (node-score-p1 node) (node-score-p2 node))
)

