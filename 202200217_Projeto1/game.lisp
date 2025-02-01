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

(defun distribute-pieces)
