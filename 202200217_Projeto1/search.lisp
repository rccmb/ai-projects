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
  "Checks if a NODE already exists in NODES according to the rules of ALGORITHM."
  (cond 
    ((eq algorithm 'bfs) 
      (cond 
        ((null (node-state node)) t) ; If the node state is null we return true, we do not want to store nil nodes.
        ((null nodes) nil) ; We reached the end of the list and did not find a match.
        ((and (not (null (node-state (car nodes)))) (compare-state node (car nodes))) t) ; If the current list node does not have a nil state and the nodes are equal, we have found a match.
        (t (node-existsp node (cdr nodes) algorithm)) ; Skip the current node and go to the next.
      )
    ) 
    ((eq algorithm 'dfs) ; Has to compare the depth of the node.
      (cond
        ((null (node-state node)) t) ; If the node state is null we return true, we do not want to store nil nodes.
        ((null nodes) nil) ; Reached the end of the list.
        ((and (not (null (node-state (car nodes)))) (compare-state node (car nodes)) (compare-depth (car nodes) node)) t) ; If the current list node does not have a nil state, the nodes are equal and the depth of the matched node is lower, we found a match.
        (t (node-existsp node (cdr nodes) algorithm)) ; No match, go to the next node.
      )
    )
    ((eq algorithm 'a-star)
      (cond 
        ((null (node-state node)) t) ; If the node state is null we return true, we do not want to store nil nodes.
        ((null nodes) nil) ; Reached the end of the list.
        ((and (not (null (node-state (car nodes)))) (compare-state node (car nodes)) (compare-cost (car nodes) node)) t) ; If the current list node does not have a nil state, the nodes are equal and the cost of the matched node is lower, we found a match.
        (t (node-existsp node (cdr nodes) algorithm)) ; No match, go to the next node.
      )
    )
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
  "Creates a new child of this NODE, using the OPERATOR and with a specific ALGORITHM & HEURISTIC, making a move in INDEX1 and INDEX2 and storing the state (board)."
  (let 
    ((state (funcall operator index1 index2 (node-state node)))) ; Passes the line and index to make a move, also passes the old board.
    (cond 
      ((null node) nil)
      (t (progn
        (set-generated-nodes (+ (get-generated-nodes) 1)) ; Increment the number of generated nodes. ; TODO : Check
        (cond 
          ((eq algorithm 'a-star) (create-node state (+ (node-depth node) 1) (funcall heuristic state) nil)) ; If the algorithm is A*, then we need to use a heuristic for the node.
          (t (create-node state (+ (node-depth node) 1) 0 nil)) ; Call the operator on the parent state, add one depth and set the passed node as the father.
        )
      ))
    )
  )
)

; TODO : Check again.
(defun generate-children (node operator algorithm &optional (max-depth 0) heuristic)
  "Goes through every line and row, creating a child for every possible move (as long as the move is valid)."
  (labels 
    (
      (children-helper (i1 i2)
        (cond 
          ((or (null node) (null operator) (> i1 1)) nil) ; We have ran through board lines.
          ((= i2 6) (children-helper (+ i1 1) 0)) ; We have ran through line indexes.
          ((null (node-state (new-child node operator algorithm i1 i2))) (children-helper i1 (+ i2 1))) ; The move is invalid, no need to keep the child.
          ((eq algorithm 'a-star) (cons (new-child node operator algorithm i1 i2 heuristic) (children-helper i1 (+ i2 1)))) ; If the algorithm is A*, we need to pass the heuristic.
          (t (cons (new-child node operator algorithm i1 i2) (children-helper i1 (+ i2 1)))) ; We create a new child and move to the next, without a heuristic.
        ))
    )
    (cond 
      ((null node) nil) ; Node is nil, we don't create any children.
      ((and (eq algorithm 'dfs) (>= (node-depth node) max-depth)) nil) ; If the current node has reached the max depth, we don't create any children.
      (t (progn 
        (set-expanded-nodes (+ (get-expanded-nodes) 1)) ; Increment the number of expanded nodes.
        (children-helper 0 0)) ; We create children for this node.
      ) 
    )
  )
)

;;; Search Algorithms.

(defun bfs-open (nodes-open nodes-children)
  "Puts the children nodes NODES-CHILDREN at the back of NODES-OPEN."
  (append nodes-open nodes-children) ; We can just append the children to the back.
)

(defun bfs (eval generator operator &optional (open '()) (closed '()))
  "Breadth-First-Search Algorithm. 11 - 3."
  (labels 
    (
      (remove-existing (l) ; Removes nodes from l that are already in closed.
        (cond
          ((null l) nil) ; If the list is null, return nil.
          ((null (node-state (car l))) (remove-existing (cdr l))) ; Skips nodes with a null state.
          ((not (node-existsp (car l) closed 'bfs)) (cons (car l) (remove-existing (cdr l)))) ; If there is not a node with the same state, we keep it.
          (t (remove-existing (cdr l))) ; Node already exists, skip it.
        ))
      (check-solution (l) ; Checks l to see if there is a solution node. 
        (cond
          ((null l) nil) ; The list is null and a solution was not found.
          ((funcall eval (car l)) (car l)) ; Call the eval function, if it returns true, we have a solution.
          (t (check-solution (cdr l))) ; Node wasn't the solution, go to the next.
        ))
    )
    (cond 
      ((null open) nil) ; If open is empty, return nil, no solution.
      (t (let* 
        (
          (first-node (car open)) ; Takes the first node from open.
          (new-closed (append closed (list first-node))) ; Place the first node in closed.
          (children (remove-existing (funcall generator first-node operator 'bfs))) ; Expand the first node and remove already existing nodes.
          (new-open (bfs-open (cdr open) children)) ; Place the generated children of the first node inside of open, while removing the first node.
          (solution-node (check-solution new-open)) ; Checks if any of the generated children were a solution node.
        ) 
        (if (null solution-node) ; Is solution node null?
          (bfs eval generator operator new-open new-closed) ; If true, runs BFS again.
          solution-node ; If the solution node was found, return it.
        )
      ))
    )
  )
)

(defun dfs-open (nodes-open nodes-children)
  "Puts the children nodes NODES-CHILDREN in front of NODES-OPEN."
  (append nodes-children nodes-open) ; We have to put the children at the front, so we append the open nodes at the back of children.
)

(defun dfs (eval generator operator max-depth &optional (open '()) (closed '()))
  "Depth-First-Search Algorithm, 17 - 3."
  (labels
    (
      (remove-existing (l) ; Removes nodes from l that are in open, since nodes in open have a lower depth.
        (cond
          ((null l) nil) ; Reached the end of the list.
          ((null (node-state (car l))) (remove-existing (cdr l))) ; State is null, we skip it.
          ((node-existsp (car l) open 'dfs) (remove-existing (cdr l))) ; The node already exists in open, we skip it.
          ((node-existsp (car l) closed 'dfs) (remove-existing (cdr l))) ; The node already exists in closed, and the one in closed has a LOWER depth, it means no solution is to be found.
          (t (cons (car l) (remove-existing (cdr l)))) ; Node does not exist, we keep it.
        ))
      (check-solution (l)
        (cond 
          ((null l) nil) ; Reached the end of the list.
          ((funcall eval (car l)) (car l)) ; Node was the solution, return it.
          (t (check-solution (cdr l))) ; Node was not the solution, go to the next.
        ))
    )
    (cond
      ((null open) nil) ; If open is empty, return nil, no solution.
      (t (let*
        (
          (first-node (car open)) ; Takes the first node in open.
          (new-closed (append closed (list first-node))) ; Put this node in closed.
          (children (funcall generator first-node operator 'dfs max-depth)) ; Generate the children of this node, will be nil if the node depth is bigger than max-depth.
        )
        (if (null children) ; Checks if any children were generated.
          (dfs eval generator operator max-depth (cdr open) new-closed) ; Could not generate any children, run DFS again, cutting the first node from open. 
          (let* ; We could generate children. 
            (
              (new-children (remove-existing children)) ; We remove duplicated or unnecessary children from the generated nodes.
              (new-open (dfs-open (cdr open) new-children)) ; We put the generated children inside of open, removing the node that generated them.
              (solution-node (check-solution new-open)) ; Checks if any of the generated children were a solution node.
            ) 
            (if (null solution-node) ; Is solution node null?
              (dfs eval generator operator max-depth new-open new-closed) ; If true, we run DFS again.
              solution-node ; False, we have found a solution.
            )
          )
        )
      ))
    )
  )
)

(defun order-nodes (original)
  "Orders nodes in ORIGINAL using the quicksort algorithm according to node cost."
  (if (or (null original) (null (cdr original))) 
      original
      (let* ((pivot (car original))
             (rest (cdr original))
             (less (remove-if-not (lambda (x) (< (node-cost x) (node-cost pivot))) rest))
             (greater (remove-if-not (lambda (x) (>= (node-cost x) (node-cost pivot))) rest)))
        (append (order-nodes less) (list pivot) (order-nodes greater))
      )
  )
)

(defun put-successors-in-open (open successors)
  "Puts the SUCCESSORS of a node in OPEN."
  (order-nodes (append open successors))
)

(defun a-star (eval generator operator heuristic &optional (open '()) (closed '()))
  "A* Algorithm, 25 - 3."
  (labels 
    (
      (remove-existing (l)
        (cond 
          ((null l) nil) ; We have reached the end of the list.
          ((null (node-state (car l))) (remove-existing (cdr l))) ; State is null, we skip it.
          ((node-existsp (car l) open 'a-star) (remove-existing (cdr l))) ; The node already exists in open and this one has a higher cost, we skip it.
          ((node-existsp (car l) closed 'a-star) (remove-existing (cdr l))) ; The node already exists in closed and this one has a higher cost, we skip it.
          (t (cons (car l) (remove-existing (cdr l)))) ; Node does not exist, we keep it.
        ))
      (check-solution (node)
        (if (funcall eval node) 
          node ; Node was the solution.
          nil ; Node was not the solution.
        ))
    )
    (cond 
      ((null open) nil) ; Open is empty, return nil.
      (t (let*
        (
          (first-node (car open)); Open is always passed ordered, therefore the node with minimum cost is the first one.
          (new-closed (append closed (list first-node))) ; Put this node in closed.
          (new-children (remove-existing (funcall generator first-node operator 'a-star 0 heuristic))) ; We remove children that already exist, and the cost of those that already exist is lower.
          (new-open (put-successors-in-open (cdr open) new-children)) ; We put the successors in open, ordered by cost.
          (solution-node (check-solution (car new-open))) ; Check if the next node is a solution node. A*.
        ) 
        (cond
          ((null new-open) nil) ; No children were put in open, there is no solution.
          ((not (null solution-node)) solution-node) ; There is a solution.
          (t (a-star eval generator operator heuristic new-open new-closed)) ; Run the algorithm again.
        )
      ))
    )
  )
)