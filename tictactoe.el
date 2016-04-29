;; tictactoe.el -- play tic tac toe in Emacs

(defun tictactoe()
  "Start playing tic tac toe"
  (interactive)
  (switch-to-buffer "tictactoe")
  (tictactoe-init)
  (tictactoe-mode))

(defun tictactoe-init ()
  "Start a new game of tic tac toe"
  (setq *tictactoe-board* (make-vector (* *tictactoe-size*
                                          *tictactoe-size*)
                                       ?\.))
  (tictactoe-print-board)
  (setq *tictactoe-current-player* ?\X))

(defun tictactoe-print-board ()
  "Display tictactoe board in current buffer"
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dotimes (row *tictactoe-size*)
      (dotimes (column *tictactoe-size*)
        (insert (tictactoe-get-square row column)))
      (insert "\n"))))

(define-derived-mode tictactoe-mode special-mode "tic-tac-toe"
  (define-key tictactoe-mode-map (kbd "SPC") 'tictactoe-mark))

(defun tictactoe-get-square (row column)
  "Get the value in the (row, column) of the tictactoe board"
  (elt *tictactoe-board*
       (+ column
          (* row
             *tictactoe-size*))))

(defun tictactoe-set-square (row column value)
  "Set the value in the (row, column) of the tictactoe board"
  (aset *tictactoe-board*
       (+ column
          (* row
             *tictactoe-size*))
       value))

(defun tictactoe-mark ()
  "mark the current square"
  (interactive)
  (let ((row (1- (line-number-at-pos)))
        (column (current-column)))
    (tictactoe-set-square row column *tictactoe-current-player*))
  (tictactoe-print-board)
  (when (tictactoe-game-has-been-won)
    (message "Congrats! player %c won!" *tictactoe-current-player*))
  (tictactoe-swap-players))

(defun tictactoe-swap-players ()
  "Make it the other player's turn"
  (if (char-equal *tictactoe-current-player* ?\X)
      (setq *tictactoe-current-player* ?\O)
    (setq *tictactoe-current-player* ?\X)))

(defun tictactoe-game-has-been-won ()
  "Returns t if the game has been won, nil otherwise"
  (or (tictactoe-diagonal-win)
      (tictactoe-row-win)
      (tictactoe-column-win)))

(defun tictactoe-diagonal-win()
  (or (tictactoe-all-same-player (tictactoe-get-square 0 0)
                                 (tictactoe-get-square 1 1)
                                 (tictactoe-get-square 2 2))))

(defun tictactoe-row-win ()
  (let ((has-won nil))
    (dotimes (row *tictactoe-size*)
      (when (tictactoe-all-same-player (tictactoe-get-square row 0)
                                       (tictactoe-get-square row 1)
                                       (tictactoe-get-square row 2))
        (setq has-won t)))
    has-won))

(defun tictactoe-column-win ()
  (let ((has-won nil))
    (dotimes (column *tictactoe-size*)
      (when (tictactoe-all-same-player (tictactoe-get-square 0 column)
                                       (tictactoe-get-square 1 column)
                                       (tictactoe-get-square 2 column))
        (setq has-won t)))
    has-won))

(defun tictactoe-all-same-player (sq1 sq2 sq3)
  (and (tictactoe-is-a-player sq1)
       (char-equal sq1 sq2)
       (char-equal sq2 sq3)))

(defun tictactoe-is-a-player (square)
  (or (char-equal square ?\X)
      (char-equal square ?\O)))

(defvar *tictactoe-board* nil
  "The board itself")

(defvar *tictactoe-current-player* nil
  "The character representing the current player")

(defconst *tictactoe-size* 3
  "the size of the board - height and width")
