;;;; jogo.lisp
;;;; Related to user interaction and interface.
;;;; Author: Rodrigo Baptista 202200217

(defparameter *game-not-finished* t "If the game is finished.")
(defparameter *current-turn* 1 "Current game turn.")

(load (merge-pathnames "algoritmo.lisp" (make-pathname :directory (pathname-directory *load-pathname*))))
(load (merge-pathnames "puzzle.lisp" (make-pathname :directory (pathname-directory *load-pathname*))))

(defun initialize ()
  (progn
    (setq *game-not-finished* t)
    (setq *current-turn* 1)
    (let 
      ((mode (read-mode)))
      (cond 
        ((eq mode 'human-vs-computer) 
          (let 
            (
              (first-player (read-first-player))
              (computer-time-limit (read-computer-time-limit))  
            )
            (human-game first-player computer-time-limit)
          )
        )
        ; ((eq mode 'computer-vs-computer) 
        ;   (let 
        ;     ((game-time-limit (read-game-time-limit)))
        ;     (ai-game game-time-limit)
        ;   )
        ; )
      )
    )
  )
)

(defun human-game (first-player computer-time-limit)
  (let 
    (
      (current-player first-player)
      (current-node (create-node (board-initial) 0 0 0 nil))  
    )
    (loop while *game-not-finished* do
      (progn
        (print-game-turn)
        (print-board (node-state current-node)) ; Before.

        (cond ; Turn logic.
          ((and (= current-player 1) (= (line-piece-count 1 (node-state current-node)) 0)) (format t "You can not move! Passing turn...~%")) ; Human can't move. 
          ((and (= current-player -1) (= (line-piece-count 0 (node-state current-node)) 0)) (format t "Computer could not move! Your turn again!~%")) ; Computer can't move. 
          (t 
            (cond 
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
                (progn
                  (format t "Computer thinking...~%")
                  (let* 
                    (
                      (search-depth 5) ; Maximum search depth allowed.
                      (alpha -1.0e+9)
                      (beta 1.0e+9)
                      (best-move nil)
                      (best-score -1.0e+9)
                    )
                    (loop for pos from 0 below 6 do
                      (let 
                        ((child (game-operator 0 pos current-node)))
                        (if (not (null child))
                          (let 
                            ((score (- (negamax child (1- search-depth) (- beta) (- alpha) (- current-player) 'generate-children 'node-solutionp 'evaluate-node 'game-operator computer-time-limit))))
                            (if (> score best-score)
                              (progn
                                (setf best-score score)
                                (setf best-move pos)
                              )
                            )
                          )
                        )
                      )
                    )
                    (if (not (null best-move))
                      (progn
                        (setf current-node (game-operator 0 best-move current-node))
                        (print-move 0 best-move)
                      )
                    )
                  )
                )
              )
          ))
        ) 

        (setq *current-turn* (+ *current-turn* 1))
        (setf current-player (* current-player -1))

        (print-board (node-state current-node)) ; After.
        (print-score (node-score-p1 current-node) (node-score-p2 current-node))
      )
    )
  )
)

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

(defun read-game-time-limit ()
  "Allows for the user to decide the time limit for computer-vs-computer games. In minutes."
  (progn
    (format t "Maximum number of minutes allowed for game: ")
    (let
      ((time-limit (read)))
      (if (and (numberp time-limit) (> time-limit 0))
        time-limit
        (read-game-time-limit)
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

; TODO, also write to log.dat
(defun print-game-turn ()
  "Prints the current game turn."
  (format t "~%TURN ~d~%" *current-turn*)
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
      ((= line 0) (format t "Player 2 turn, moved pieces in hole ~d.~%" (+ position 1)))
      ((= line 1) (format t "Player 1 turn, moved pieces in hole ~d.~%" (+ position 1)))
    )
  )
)

(defun set-game-finished ()
  (setq *game-not-finished* nil)
)