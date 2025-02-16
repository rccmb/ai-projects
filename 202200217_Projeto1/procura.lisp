;;;; search.lisp
;;;; Independent from the problem, search methods BFS, DFS and A* and helpers for the search methods.
;;;; Author: Rodrigo Baptista 202200217

;;; Domain Independent Node Functions.

(defun create-node (state depth heuristic previous)
  (list state depth heuristic previous)
)

(defun node-state (node)
  (cond 
    ((null node) nil)
    (t (nth 0 node))
  )
)

(defun node-depth (node)
  (cond 
    ((null node) nil)
    (t (nth 1 node))
  )
)

(defun node-heuristic (node)
  (cond 
    ((null node) nil)
    (t (nth 2 node))
  )
)

(defun node-previous (node)
  (cond
    ((null node) nil)
    (t (nth 3 node))
  )
)

(defun node-cost (node)
  (cond 
    ((null node) nil)
    (t (+ (node-depth node) (node-heuristic node)))
  )
)

;;; Helper functions.

(defun order-nodes (original)
  "Orders nodes in ORIGINAL using the built-in stable-sort function according to node cost."
  (stable-sort original #'< :key #'node-cost)
)

(defun remove-existing (l closed algorithm)
  "Removes nodes from L that are already in CLOSED."
  (remove-if #'(lambda (node) (or (null (node-state node)) (node-existsp node closed algorithm))) l)
)

(defun check-solution (l objective)
  "Returns the first node in L that satisfies OBJECTIVE, or NIL if none do."
  (find-if objective l)
)

;;; Search Algorithms.

(defun bfs (initial-node objective generator operator)
  "Breadth-First-Search Algorithm. 11 - 3. Receives an INITIAL-NODE, the OBJECTIVE state, the GENERATOR function and the game OPERATOR."
  (let 
    (
      (open (list initial-node))
      (closed '())
    )
    (loop while open do
      (let* 
        (
          (first-node (pop open)) ; Take first node and remove from open.
          (children (remove-existing (funcall generator first-node operator 'bfs) closed 'bfs)) ; Generate children.
          (solution-node (check-solution children objective)) ; Check for a solution.
        )
        (when solution-node (return solution-node))
        (push first-node closed)
        (setf open (append open children)) ; BFS-OPEN.
      )
    )
  )
)

(defun dfs (initial-node objective generator operator max-depth)
  "Depth-First-Search Algorithm. 17 - 3. Receives an INITIAL-NODE, the OBJECTIVE state, the GENERATOR function, the game OPERATOR and the MAX-DEPTH allowed for the search."
  (let 
    (
      (open (list initial-node))
      (closed '())
    )
    (loop while open do
      (let*
        (
          (first-node (pop open)) ; Take first node and remove from open.
          (children (remove-existing (funcall generator first-node operator 'dfs max-depth) closed 'dfs)) ; Generate children.
          (solution-node (check-solution children objective)) ; Check for a solution.
        )
        (when solution-node (return solution-node))
        (push first-node closed)
        (setf open (append children open)) ; DFS-OPEN.
      )
    )
  )
)

(defun a-star (initial-node objective generator operator heuristic)
  "A* Algorithm. 25 - 3. Receives an INITIAL-NODE, the OBJECTIVE state, the GENERATOR function, the game OPERATOR and an HEURISTIC function."
  (let 
    (
      (open (list initial-node))
      (closed '())
    )
    (loop while open do
      (let*
        (
          (first-node (pop open)) ; Take first node and remove from open.
          (children (remove-existing (funcall generator first-node operator 'a-star 0 heuristic) closed 'a-star)) ; Generate children.
          (solution-node (check-solution (list first-node) objective)) ; Check for a solution.
        )
        (when solution-node (return solution-node))
        (push first-node closed)
        (setf open (order-nodes (append open children))) ; A* PUT-SUCCESSORS-IN-OPEN.
      )
    )
  )
)