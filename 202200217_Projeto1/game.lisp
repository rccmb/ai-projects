;;;; game.lisp
;;;; Related to the domain of the game, operators, heuristics.
;;;; Author: Rodrigo Baptista 202200217


;;; Boards

(defun board-empty (&optional (rows 2) (columns 6))
  "Returns a 2x6 board with all cells empty."
  (make-list rows :initial-element (make-list columns :initial-element '0))
)

(defun board-test ()
  "Returns a 2x6 that corresponds to exercise d)."
  '((1 2 3 4 5 6)
    (6 5 4 3 2 1))
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

(defun replace-position (index line &optional (value 0))
  "Replaces the value at INDEX in the LINE with VALUE. Line based."
  (labels 
    ((go-through-rep-pos (index line)
      "Recursively goes through the LINE and replaces the value at INDEX."
      (cond
        ((null line) nil)
        ((= index 0) (cons value (go-through-rep-pos (- index 1) (cdr line)))) ; We replace this value.
        (t (cons (car line) (go-through-rep-pos (- index 1) (cdr line))))
      ))
    )
    (if (valid-line-indexp index)
      (go-through-rep-pos index line)
      nil)
  )
)

(defun replace-value (index1 index2 board &optional (value 0))
  "Replaces the value at line INDEX1 and index INDEX2 of the BOARD with VALUE. Board based."
  (labels 
    ((go-through-rep-val (index board) 
      "Recursively goes through the BOARD and replaces the line at INDEX."
      (cond
        ((null board) nil)
        ((= index 0) (cons (replace-position index2 (car board) value) (go-through-rep-val (- index 1) (cdr board)))) ; We replace this line.
        (t (cons (car board) (go-through-rep-val (- index 1) (cdr board))))
      ))
    )
    (if (and (valid-linep index1) (valid-line-indexp index2))
      (go-through-rep-val index1 board)
      nil
    )
  )
)

(defun increment-value (index1 index2 board)
  "Increments the value at line INDEX1 and index INDEX2 of the BOARD by 1."
  (let 
    ((cell-value (cell index1 index2 board)))
    (if (null cell-value)
      nil
      (replace-value index1 index2 board (+ cell-value 1))
    )
  )
)

;; Operators

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

(defun game-operator (index1 index2 board)
  "Denotes the play is to be made at line INDEX1 and index INDEX2 of the BOARD."
  (labels 
    (
      (change-board (holes-list b)
        "Executes changes made to the board where H are the holes to increment pieces and B the initial board."
        (let* 
          ( 
            (current-position (car holes-list))
            (line (nth 0 current-position))
            (column (nth 1 current-position))
            (next (cdr holes-list))
          )
          (cond 
            ((null holes-list) b) ; Return the changed board.
            ((and (null next) (or (= (+ (cell line column b) 1) 1) (= (+ (cell line column b) 1) 3) (= (+ (cell line column b) 1) 5))) (replace-value line column b 0)) ; Final is 1, 3 or 5, to 0.
            (t (change-board (cdr holes-list) (increment-value line column b))) ; Go to the next hole with the new board.
          )
        )
      ))
    (if (and (valid-linep index1) (valid-line-indexp index2) (not (null board)))
      (let* 
        (
          (total-pieces (cell index1 index2 board))
          (holes (distribute-pieces total-pieces index1 index2))
        )
        (if (= total-pieces 0)
          nil ; Invalid operation.
          (replace-value index1 index2 (change-board holes board) 0) ; Turns the moved value to zero and returns the new board.
        )
      )
      nil
    )
  )
)