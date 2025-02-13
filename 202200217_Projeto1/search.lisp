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

(defun node-existsp (node nodes algorithm)
  "Checks if NODE exists in NODES based on the ALGORITHM criteria."
  (cond
    ((null (node-state node)) t)  ; If node state is null, return true (avoid storing nil nodes).
    ((null nodes) nil)  ; If the list of nodes is empty, return false.
    ((eq algorithm 'bfs) (some (lambda (x) (compare-state node x)) nodes)) ; Exists if states match.
    ((eq algorithm 'dfs) (some (lambda (x) (and (compare-state node x) (compare-depth x node))) nodes))  ; Match state & depth.
    ((eq algorithm 'a-star) (some (lambda (x) (and (compare-state node x) (compare-cost x node))) nodes))  ; Match state & cost.
    (t nil)
  )
)

;;; Comparing Nodes.

(defun compare-cost (lower higher)
  "Compares the cost of a node. Returns true if LOWER has lower cost than HIGHER, nil otherwise."
  (let 
    ((lower-cost (node-cost lower))
      (higher-cost (node-cost higher)))
    (cond 
      ((null lower-cost) nil) ; If the lower cost is nil, then we assume only higher exists.
      ((null higher-cost) t) ; If the higher cost is nil, then we assume only lower exists.
      ((>= higher-cost lower-cost) t) ; Return true, the "lower" node has lower cost.
      ((<= higher-cost lower-cost) nil) ; Return nil, the "lower" node has higher cost.
    )
  )
)

(defun compare-depth (lower higher)
  "Compares the depth of a node. Returns true if LOWER has lower depth than HIGHER, nil otherwise."
  (let 
    ((lower-depth (node-depth lower))
      (higher-depth (node-depth higher)))
    (cond 
      ((null lower-depth) nil) ; If the lower depth is nil, then we assume only higher exists.
      ((null higher-depth) t) ; If the higher depth is nil, then we assume only lower exists.
      ((>= higher-depth lower-depth) t) ; Return true, the "lower" node has lower depth.
      ((<= higher-depth lower-depth) nil) ; Return nil, the "lower" node has higher depth.
    )
  )
)

;;; Node Generation.

(defun new-child (node operator algorithm index1 index2 &optional heuristic)
  "Creates a new child node using OPERATOR at INDEX1 (line) and INDEX2 (position), considering ALGORITHM & HEURISTIC."
  (when node
    (let 
      ((state (funcall operator index1 index2 (node-state node))))
      (when state  
        (set-generated-nodes (+ (get-generated-nodes) 1))
        (create-node 
          state ; Current problem state.
          (+ (node-depth node) 1) ; Depth.
          (if (eq algorithm 'a-star) ; A* is informed.
            (funcall heuristic state) 
            0
          ) 
          node ; Parent.
        )
      )
    )
  )
)  

(defun generate-children (node operator algorithm &optional (max-depth 0) heuristic)
  "Generates all valid children of NODE using OPERATOR."
  (when 
    (and 
      (not (null node)) 
      (not (and (eq algorithm 'dfs) (>= (node-depth node) max-depth))) ; If DFS, depth? 
    ) 
    (set-expanded-nodes (+ (get-expanded-nodes) 1)) 
    (let 
      ((children '())) 
      (loop for i1 from 0 to 1 do ; Board rows.
        (loop for i2 from 0 below 6 do ; Board columns.
          (let 
            ((child (new-child node operator algorithm i1 i2 heuristic)))
            (when child (push child children))
          )
        )
      )
      (nreverse children)
    )
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