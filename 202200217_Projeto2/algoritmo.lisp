;;;; algoritmo.lisp
;;;; Independent from the problem, search method NEGAMAX to calculate the best move.
;;;; Author: Rodrigo Baptista 202200217

(defun create-node (state score-p1 score-p2 previous)
  (list state score-p1 score-p2 previous)
)

(defun node-state (node)
  (cond 
    ((null node) nil)
    (t (nth 0 node))
  )
)

(defun node-score-p1 (node)
  (nth 1 node)
)

(defun node-score-p2 (node)
  (nth 2 node)
)

(defun node-previous (node)
  (nth 3 node)
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

(defun negamax (node depth alpha beta player generator objective evaluation game-operator &optional search-time)
  "Receives a NODE, a max search DEPTH, value of ALPHA, BETA, current PLAYER, the GENERATOR function for the children, the OBJECTIVE node, an EVALUATION function and the GAME-OPERATOR."
  
)
