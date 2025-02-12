;;;; jogo.lisp
;;;; Related to user interaction and interface.
;;;; Author: Rodrigo Baptista 202200217

(defparameter *game-not-finished* t "If the game is finished.")
(defparameter *player-one-score* 0 "Player 1 score.")
(defparameter *player-two-score* 0 "Player 2 score.")

(load (merge-pathnames "algoritmo.lisp" (make-pathname :directory (pathname-directory *load-pathname*))))
(load (merge-pathnames "puzzle.lisp" (make-pathname :directory (pathname-directory *load-pathname*))))

(defun initialize ()
  (progn
    (setq *game-not-finished* t)
    (setq *player-one-score* 0)
    (setq *player-two-score* 0)
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
        ((eq mode 'computer-vs-computer) 
          (let 
            ((game-time-limit (read-game-time-limit)))
            (ai-game game-time-limit)
          )
        )
      )
    )
  )
)

(defun human-game (first-player computer-time-limit)
  (let 
    (
      (current-player first-player)
      (current-board (board-initial))  
    )
    (loop while *game-not-finished* do
      (progn
        (print-board current-board) 

        (cond ; Check if the current player can move.
          ((and (= current-player 1) (= (line-piece-count 1 current-board) 0)) ; Human can't move.
            (progn
              (format t "You can not move! Passing turn...~%")
              (setf current-player 2)
            ))
          ((and (= current-player 2) (= (line-piece-count 0 current-board) 0)) ; Computer can't move.
            (progn
              (format t "Computer could not move! Your turn again!~%")
              (setf current-player 1)
            ))
        ) 

        (cond ; Current turn.
          ((= current-player 1) ; Human to play.
            (let
              ((human-move (read-human-move current-board))) ; Only valid moves get returned.
              (progn
                (setf current-board (game-operator 1 human-move current-board))
                (print-move 1 human-move)
              )
            )) 
          ((= current-player 2) ; Computer to play.
            ()) ; HERE TO MAKE MOVE
        )
        
        ; (if (= current-player 1) ; Change the player.
        ;   (setf current-player 2)
        ;   (setf current-player 1)
        ; )

        (print-score)
      )
    )
  )
)

; (defun ai-game (game-time-limit)
;   (let 
;     (
;       (current-player first-player)
;       (current-board (board-initial))  
;     )
;     (loop while *game-not-finished* do
;       (progn
;         (print-board current-board) 

;         (cond ; Check if the current player can move.
;           ((and (= current-player 1) (= (line-piece-count 1 current-board) 0)) (setf current-player 2))
;           ((and (= current-player 2) (= (line-piece-count 0 current-board) 0)) (setf current-player 1))
;         ) 

;         (cond ; Current turn.
;           ((= current-player 1)
;             ()) 
;           ((= current-player 2)
;             ()) 
;         )

;         (if (= current-player 1) ; Change the player.
;           (setf current-player 2)
;           (setf current-player 1)
;         )
;       )
;     )
;   )
; )

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
        ((= player 2) 2)
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

(defun read-human-move (board)
  "Allows for the human to make a move."
  (labels
    (
      (read-hole () 
        "Gets the index of the chosen hole."
        (progn 
          (format t "Choose a hole to move [1, 6]: ~%")
          (let
            ((hole-chosen (read)))
            (if (and (numberp hole-chosen) (> hole-chosen 0) (< hole-chosen 7) (not (= (cell 1 (- hole-chosen 1) board) 0))) 
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
(defun print-board (board)
  "Prints the current game state (BOARD)."
  (progn 
    (format t "Board:~%")
    (format t "2 - ~A~%" (nth 0 board))
    (format t "1 - ~A~%" (nth 1 board))
  )
)

; TODO, also write to log.dat
(defun print-score ()
  (progn 
    (format t "Player 1 score is: ~d~%" *player-one-score*)
    (format t "Player 2 score is: ~d~%" *player-two-score*)
    (cond
      ((< *player-one-score* *player-two-score*) (format t "Player 1 is losing by ~d points.~%" (- *player-two-score* *player-one-score*)))
      ((= *player-one-score* *player-two-score*) (format t "Player 1 and Player 2 are tied!~%"))
      ((> *player-one-score* *player-two-score*) (format t "Player 2 is losing by ~d points.~%" (- *player-one-score* *player-two-score*)))
    )
  )
)

; TODO, also write to log.dat
(defun print-move (line position)
  (progn 
    (cond 
      ((= line 0) (format t "Player 2 turn, moved pieces in hole ~d.~%" position))
      ((= line 1) (format t "Player 1 turn, moved pieces in hole ~d.~%" position))
    )
  )
)

(defun set-game-finished ()
  (setq *game-not-finished* nil)
)

(defun set-player-one-score (value)
  (setq *player-one-score* value)
)

(defun get-player-one-score ()
  *player-one-score*
)

(defun set-player-two-score (value)
  (setq *player-two-score* value)
)

(defun get-player-two-score ()
  *player-two-score*
)