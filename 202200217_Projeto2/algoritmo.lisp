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

(defun negamax (node depth alpha beta player generator objective evaluation game-operator &optional (start-time -1) (time-limit -1))
  "Receives a NODE, a max search DEPTH, value of ALPHA, BETA, current PLAYER, the GENERATOR function for the children, the OBJECTIVE node, an EVALUATION function and the GAME-OPERATOR. 34 - 4."
  (if (or (= depth 0) (funcall objective node) (and (not (= time-limit -1)) (elapsed-sufficient start-time time-limit)))
    (* player (funcall evaluation node)) ; Objective node or final search depth. Human VS Computer time limit.
    (let 
      (
        (max-value -1.0e+9)
        (children (funcall generator node game-operator player))
      )
      (progn 
        (setf children (sort children #'> :key (lambda (child) (* player (funcall evaluation child)))))
        (dolist (child children)
          (let 
            ((score (- (negamax child (- depth 1) (- beta) (- alpha) (- player) generator objective evaluation game-operator start-time time-limit))))
            (set-nodes-analyzed (+ (get-nodes-analyzed) 1))
            (when (> score max-value) (setf max-value score))
            (when (> score alpha) 
              (setf alpha score)
              (set-alpha-cuts (+ (get-alpha-cuts) 1))
            )
            (when (>= alpha beta) 
              (set-beta-cuts (+ (get-beta-cuts) 1))
              (return max-value) ; Prune.
            )
          )
        )
        max-value
      )
    )
  )
)

(defun elapsed-sufficient (start limit)
  "Determines if the time LIMIT has been passed according to the START time and current time."
  (let*
    (
      (current (get-internal-real-time))
      (elapsed (- current start))
    )
    (> elapsed (* limit 1000))
  )
)