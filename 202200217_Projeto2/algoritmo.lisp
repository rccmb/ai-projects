;;;; algoritmo.lisp
;;;; Independent from the problem, search method NEGAMAX to calculate the best move.
;;;; Author: Rodrigo Baptista 202200217

(defun create-node (state score-p1 score-p2 depth previous)
  (list state score-p1 score-p2 depth previous)
)

(defun node-state (node)
  (cond 
    ((null node) nil)
    (t (nth 0 node))
  )
)

(defun node-score-p1 (node)
  (cond 
    ((null node) nil)
    (t (nth 1 node))
  )
)

(defun node-score-p2 (node)
  (cond 
    ((null node) nil)
    (t (nth 2 node))
  )
)

(defun node-depth (node)
  (cond 
    ((null node) nil)
    (t (nth 3 node))
  )
)

(defun node-previous (node)
  (cond 
    ((null node) nil)
    (t (nth 4 node))
  )
)

(defun new-child (node operator index1 index2)
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

(defun negamax (node depth alpha beta player generator objective evaluation game-operator)
  "Receives a NODE, a max search DEPTH, value of ALPHA, BETA, current PLAYER, the GENERATOR function for the children, the OBJECTIVE node, an EVALUATION function and the GAME-OPERATOR. 34 - 4."
  (if (or (= depth 0) (funcall objective node))
    (* player (funcall evaluation node))
    (block nil
      (let 
        (
          (max-value -1.0e+9)
          (children (funcall generator node game-operator player))
        )
        (dolist (child children)
          (let 
            ((score (- (negamax child (1- depth) (- beta) (- alpha) (- player) generator objective evaluation game-operator))))
            (when (> score max-value) (setf max-value score))
            (when (> score alpha) (setf alpha score))
            (when (>= alpha beta) (return max-value))
          )
        )
        max-value
      )
    )
  )
)
