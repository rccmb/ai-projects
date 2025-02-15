;;;; jogo.lisp
;;;; Related to user interaction and interface.
;;;; Author: Rodrigo Baptista 202200217

(defconstant *search-depth* 10 "Maximum search depth allowed for negamax.")

(defparameter *current-turn* 1 "Current game turn.")
(defparameter *hash-table* (make-hash-table) "Memoization, stores previous game moves.")
(defparameter *hash-table-hit-rate* 0 "Hit rate for the hash table.")
(defparameter *hash-table-miss-rate* 0 "Miss rate for the hash table.")
(defparameter *alpha-cuts* 0 "Number of alpha cuts, current run.")
(defparameter *beta-cuts* 0 "Number of beta cuts, current run.")
(defparameter *alpha-cuts-total* 0 "Number of alpha cuts, total through match.")
(defparameter *beta-cuts-total* 0 "Number of beta cuts, total through match.")

(load (merge-pathnames "algoritmo.lisp" (make-pathname :directory (pathname-directory *load-pathname*))))
(load (merge-pathnames "puzzle.lisp" (make-pathname :directory (pathname-directory *load-pathname*))))

(defun initialize ()
  (progn
    (setq *current-turn* 1)
    (setq *hash-table* (make-hash-table))
    (setq *hash-table-hit-rate* 0)
    (setq *hash-table-miss-rate* 0)
    (setq *alpha-cuts* 0)
    (setq *beta-cuts* 0)
    (setq *alpha-cuts-total* 0)
    (setq *beta-cuts-total* 0)
    (let 
      ((mode (read-mode)))
      (cond 
        ((eq mode 'human-vs-computer) 
          (let 
            (
              (first-player (read-first-player))
              (computer-time-limit (read-computer-time-limit))  
            )
            (game first-player mode computer-time-limit)
          )
        )
        ((eq mode 'computer-vs-computer) 
          (let 
            ((computer-time-limit (read-computer-time-limit)))
            (game 1 mode computer-time-limit)
          )
        )
      )
    )
  )
)

(defun game (first-player mode &optional (computer-time-limit 0))
  (let 
    (
      (current-player first-player)
      (current-node (create-node (board-initial) 0 0 0 nil))  
    )
    (loop while t do
      (progn
        (print-game-turn)
        (print-board (node-state current-node)) ; Before.
        
        (cond
          ((and (= current-player 1) (= (line-piece-count 1 (node-state current-node)) 0)) (print-cant-move current-player)) ; Player 1 can't move. 
          ((and (= current-player -1) (= (line-piece-count 0 (node-state current-node)) 0)) (print-cant-move current-player)) ; Player 2 can't move.
          (t 
            (if (eq mode 'human-vs-computer)
              (cond ; Human VS Computer
                ((= current-player 1) ; Human to play.
                  (let
                    ((human-move (read-human-move (node-state current-node) current-player))) ; Only valid moves get returned.
                    (progn
                      (setf current-node (game-operator 1 human-move current-node))
                      (print-move current-player human-move)
                    )
                  )
                ) 
                ((= current-player -1) ; Computer to play.
                  (let 
                    ((best-move (read-computer-move current-player current-node computer-time-limit)))
                    (if (not (null best-move))
                      (progn
                        (setf current-node (game-operator 0 best-move current-node))
                        (print-move 0 best-move)
                      )
                    )
                  )
                )
              )
              (cond ; Computer VS Computer
                ((= current-player 1)
                  (let 
                    ((best-move (read-computer-move current-player current-node computer-time-limit)))
                    (if (not (null best-move))
                      (progn
                        (setf current-node (game-operator 1 best-move current-node))
                        (print-move 1 best-move)
                      )
                    )
                  )
                ) 
                ((= current-player -1)
                  (let 
                    ((best-move (read-computer-move current-player current-node computer-time-limit)))
                    (if (not (null best-move))
                      (progn
                        (setf current-node (game-operator 0 best-move current-node))
                        (print-move 0 best-move)
                      )
                    )
                  )
                )
              )
            ) 
          )
        )
        
        (setq *current-turn* (+ *current-turn* 1))
        (setf current-player (* current-player -1))

        (print-board (node-state current-node)) ; After.
        (print-score (node-score-p1 current-node) (node-score-p2 current-node))

        (if (board-emptyp (node-state current-node)) 
          (progn 
            (print-game-over current-node computer-time-limit)
            (return)
          )
        )
      )
    )
  )
)

;;; Reading various forms of input.

(defun read-mode ()
  "Allows the user to choose the mode."
  (progn
    (format t "What mode? ~%")
    (format t "1 - Human VS Computer ~%")
    (format t "2 - Computer VS Computer ~%")
    (format t "Mode: ")
    (let 
      ((mode (read)))
      (cond 
        ((= mode 1) 'human-vs-computer)
        ((= mode 2) 'computer-vs-computer)
        (t (read-mode))
      )
    )
  )
)

(defun read-first-player ()
  "Allows the user to decide who starts the game."
  (progn
    (format t "Who starts the game? ~%")
    (format t "1 - Human ~%")
    (format t "2 - Computer ~%")
    (format t "First Player: ")
    (let 
      ((player (read)))
      (cond 
        ((= player 1) 1)
        ((= player 2) -1)
        (t (read-first-player))
      )
    )
  )
)

(defun read-computer-time-limit ()
  "Allows for the user to decide the time limit for computer moves. Between 1 and 20 seconds."
  (progn
    (format t "Maximum number of seconds allowed for computer move [1, 20]: ")
    (let
      ((time-limit (read)))
      (if (and (numberp time-limit) (> time-limit 0) (< time-limit 21))
        time-limit
        (read-computer-time-limit)
      )
    )
  )
)

(defun read-human-move (board current-player)
  "Allows for the human to make a move."
  (labels
    (
      (read-hole () 
        "Gets the index of the chosen hole."
        (progn 
          (format t "Choose a hole to move [1, 6]: ~%")
          (let
            ((hole-chosen (read)))
            (if (and (numberp hole-chosen) (> hole-chosen 0) (< hole-chosen 7) (not (= (cell (if (= current-player 1) current-player 0) (- hole-chosen 1) board) 0))) 
              (- hole-chosen 1) ; Correct to index.
              (progn 
                (format t "Invalid choice, hole needs to be in the interval [1, 6] and must not have 0 pieces.~%")
                (read-hole)
              )
            )
          )
        )
      )
    )
    (let
      ((hole (read-hole)))
      hole
    )
  )
)

(defun read-computer-move (current-player current-node &optional (computer-time-limit -1))
  (progn
    (format t "Computer thinking...~%")
    (let* 
      (
        (alpha -1.0e+9)
        (beta 1.0e+9)
        (best-move nil)
        (best-score -1.0e+9)
        (line (if (= current-player 1) 1 0))
        (hash-key (list current-player (node-state current-node)))
        (start-time (get-internal-real-time))
      )
      (progn 
        (if (gethash hash-key *hash-table*)
          (progn ; In hash table.
            (setf best-move (gethash hash-key *hash-table*))
            (setq *hash-table-hit-rate* (+ *hash-table-hit-rate* 1))
          ) 
          (progn ; Not in hash table.
            (loop for pos from 0 below 6 do 
              (let 
                ((child (game-operator line pos current-node)))
                (if (not (null child))
                  (let 
                    ((score (- (negamax child *search-depth* (- beta) (- alpha) (- current-player) 'generate-children 'node-solutionp 'evaluate-node 'game-operator start-time computer-time-limit))))
                    (if (>= score best-score)
                      (progn
                        (setf best-score score)
                        (setf best-move pos)
                      )
                    )
                  )
                )
              )
            )
            (setf (gethash hash-key *hash-table*) best-move) ; Memorize the best move.
            (setq *hash-table-miss-rate* (+ *hash-table-miss-rate* 1))
          )
        )
        (print-cuts)
        (setq *alpha-cuts-total* (+ *alpha-cuts* *alpha-cuts-total*))
        (setq *alpha-cuts* 0)
        (setq *beta-cuts-total* (+ *beta-cuts* *beta-cuts-total*))
        (setq *beta-cuts* 0)
        best-move
      )
    )
  )
)

;;; Writing to screen and log.dat

(defun print-cant-move (current-player)
  (if (= current-player 1)
    (format t "Player 1 can't move! Passing turn...~%")
    (format t "Player 2 can't move! Passing turn...~%")
  )
)

; TODO, also write to log.dat
(defun print-game-turn ()
  "Prints the current game turn."
  (format t "~%----- TURN ~d -----~%" *current-turn*)
)

; TODO, also write to log.dat
(defun print-board (board)
  "Prints the current game state (BOARD)."
  (progn 
    (format t "Board:~%")
    (format t "2 - ~A~%" (nth 0 board))
    (format t "1 - ~A~%" (nth 1 board))
  )
)

; TODO, also write to log.dat
(defun print-score (score-p1 score-p2)
  (progn 
    (format t "Score:~%")
    (format t "- Player 1 score is: ~d~%" score-p1)
    (format t "- Player 2 score is: ~d~%" score-p2)
    (cond
      ((< score-p1 score-p2) (format t "Player 1 is losing by ~d points.~%" (- score-p2 score-p1)))
      ((= score-p1 score-p2) (format t "Player 1 and Player 2 are tied!~%"))
      ((> score-p1 score-p2) (format t "Player 2 is losing by ~d points.~%" (- score-p1 score-p2)))
    )
  )
)

; TODO, also write to log.dat
(defun print-move (line position)
  (progn 
    (cond 
      ((= line 0) (format t "Player 2 moved pieces in hole ~d.~%" (+ position 1)))
      ((= line 1) (format t "Player 1 moved pieces in hole ~d.~%" (+ position 1)))
    )
  )
)

(defun print-cuts ()
  (progn
    (format t "Number of alpha cuts: ~d~%" *alpha-cuts*)
    (format t "Number of beta cuts: ~d~%" *beta-cuts*)
  )
)

; TODO, also write to log.dat
(defun print-game-over (game-node computer-time-limit)
  (let 
    (
      (score-p1 (node-score-p1 game-node))
      (score-p2 (node-score-p2 game-node))
      (game-depth (node-depth game-node))
    )
    (progn 
      (format t "~%----- GAME OVER -----~%")
      (format t "No more pieces left to capture!~%")
      (format t "The final score is: ~%")
      (format t "- Player 1 score is: ~d~%" score-p1)
      (format t "- Player 2 score is: ~d~%" score-p2)
      (cond
        ((< score-p1 score-p2) (format t "Player 2 WINS!~%Player 1 lost by ~d points.~%" (- score-p2 score-p1)))
        ((= score-p1 score-p2) (format t "TIE!~%Player 1 and Player 2 tied!~%"))
        ((> score-p1 score-p2) (format t "Player 1 WINS!~%Player 2 lost by ~d points.~%" (- score-p1 score-p2)))
      )
      (format t "A total of ~d moves (game depth) were made over ~d turns.~%" game-depth *current-turn*)
      (format t "Hash table hit rate: ~d/~d. ~%" *hash-table-hit-rate* *hash-table-miss-rate*)
      (format t "Total number of alpha cuts: ~d~%" *alpha-cuts-total*)
      (format t "Total number of beta cuts: ~d~%" *beta-cuts-total*)
      (format t "Seconds per move for the computer: ~d seconds.~%" computer-time-limit)
      (format t "Maximum search depth for negamax: ~d~%" *search-depth*)
    )
  )
)

(defun get-alpha-cuts ()
  *alpha-cuts*
)

(defun set-alpha-cuts (value)
  (setq *alpha-cuts* value)
)

(defun get-beta-cuts ()
  *beta-cuts*
)

(defun set-beta-cuts (value)
  (setq *beta-cuts* value)
)