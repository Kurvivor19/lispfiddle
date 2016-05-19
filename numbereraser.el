;;; numbereraser.el -- game where one needs to "gather/erase" numbers

;; Copyright 2016 Ivan Truskov

;; Author: Ivan Truskov <trus19@gmail.com>
;; Version: 1

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; based on this: http://kids.quicksave.su/18604-cifrovaya-polosa.html

;; Field is filled in numbers in "crossword" patterb
;; there are 2 movable syncs that gather number values from cells
;; from every cell value may be collected given number of times down to the zero
;; some cells cannot be traversed at all
;; syncs cannot be moved through each other
;; syncs can collect up to some value; game is won when all values from cells are collected and all syncs are zero

;;; Code:

(defvar *neraser-rows* nil
  "Vertical size of current board")

(defvar *neraser-columns* nil
  "Horizontal size of current board")

(defvar *neraser-initial-board* nil
  "Initial state of the game board

Initial board state is immutable. it is used to reset board back whenever need arises")

(defvar *neraser-board* nil
  "Current state of the game board")

(defvar *neraser-step-counter* 0
  "Number of steps plaer has taken")

(defvar *neraser-sync-row* 0
  "Row of currently active sync")

(defvar *neraser-sync-column* 0
  "Column of currently active sync")

(defconst *neraser-cell-width* 4
  "number of characters in cell border on board, not counting corners")

(defconst *neraser-cell-height* 4
  "number of lines in board cell including upper border (but not lower)")

(defvar *neraser-default-maps*
  '(([4 1]
     [(*sync* . 7)
      (*cell* . 3)
      (*cell* . 2)
      (*cell* . 2)])
    ([3 2]
     [(*cell* . 1)  (*cell* . 4)
      (*cell* . [1 2])  (*cell* . [3 2])
      (*sync* . 8)  (*sync* . 5)])
    ([3 3]
     [nil     (*sync* . 15)    nil
      (*cell* . 2)  (*cell* . [6 2])  (*cell* . 3)
      (*cell* . 1)  (*sync* . 9)    (*cell* . 6)]))
  "Test game maps

Game map is a list of 2 vectors. First vector is [row column] - size of the board
Second vector represents board. its element have form:
 - nil for an empty board element
 - (type . value) for nonempty elements where
    * type is one of the symbols: *sync*, *cell*
    * value is either an integer > 0, or for cells, a vector of 2 elements [value number_of_repeats]")

(defun nthelt (N ARRAY)
  "Behaves like nth but for arrays"
  (if (and (>= N 0)
           (< N (length ARRAY)))
      (elt ARRAY N)
    nil))

(defmacro dovector (BIND &rest BODY)
  "Perform iteration over vectors

(dovector (INDEX (ELT VECTOR) [(ELT VECTOR)...]) BODY...)
INDEX is number of element
ELT is current element of a vector
Iteration goes until the end of any vector has been reached"

  (let ((isym (car BIND))
        (vsyms (cdr BIND)))
    `(condition-case nil
       (do ((,isym 0 (1+ ,isym)))
           ((>= ,isym (length ,@(cdar vsyms))))
           (let ,(mapcar (lambda (binding) `(,(car binding) (elt ,@(cdr binding) ,isym))) vsyms)
             ,@BODY))
       (args-out-of-range nil))))

(defun neraser-load-map (mapval)
  "Load map value

Map value format is desribed in *neraser-default-maps*
Sets values of *neraser-board*, *neraser-initial-board*, *neraser-columns*, *neraser-rows* according to it
Map is loaded in normalised way, that is: board is extended with nils until it has correct dimensions,
all cells are transformed to have a vector with times in it, i.e. 5 -> [5 1]

Returns nil if load failed, t otherwise"
  (if (and (listp mapval) mapval)
      (let ((dims (car mapval))
            (board (cadr mapval)))
        (if (and (arrayp dims)
                 (arrayp board))
            (block loader
              ;; retrieve dimensions
              ;; they must be greater then 1
              (setf *neraser-rows* (or (nthelt 0 dims) 1))
              (setf *neraser-columns* (or (nthelt 1 dims) 1))
              (setf *neraser-initial-board* (make-vector (* *neraser-rows* *neraser-columns*) nil))
              (dovector (i (initel *neraser-initial-board*) (loadel board))
                        (if (and (listp loadel) loadel)
                            (cl-case (car loadel)
                             ;; sync must have an integer value greater then 0 in its second cons cell
                             ('*sync*
                              (if (and (integerp (cdr loadel))
                                       (>= (cdr loadel) 1))
                                  (aset *neraser-initial-board* i loadel)
                                (print (format "Sync element without proper value at index %d\n" i))
                                (return-from loader nil)))
                             ;; cell has either a positive integer or a 2-element vector
                             ('*cell*
                              (cond
                               ((integerp (cdr loadel))
                                (if (>= (cdr loadel) 1)
                                    (aset *neraser-initial-board* i `(*cell* . [,(cdr loadel) 1]))
                                  (print (format "Cell element without proper value at index %d \n" i))
                                  (return-from loader nil)))
                               ((and (arrayp (cdr loadel))
                                     (> (length (cdr loadel)) 1))
                                (if (and (>= (elt (cdr loadel) 0) 1)
                                         (> (elt (cdr loadel) 1) 0))
                                    (aset *neraser-initial-board* i `(*cell* . ,(subseq (cdr loadel) 0 2)))
                                  (print (format "Cell element without proper value at index %d\n" i))
                                  (return-from loader nil)))
                               (t
                                (print (format "Cell element without proper value at index %d\n" i))
                                (return-from loader nil)))))
                             
                          (print (format "Unexpected board element at index %d\n" i))
                          (return-from loader nil)))
              (setf *neraser-board* (copy-tree *neraser-initial-board* t))
              ;; return value here
              t)
          nil))
    nil))

(defun neraser-get-cell (row column)
  "Get board cell with given zero-based coordinates

In case coordinates are wrong in some way, returns nil"
  (and (>= row 0)
       (>= column 0)
       *neraser-rows*
       *neraser-columns*
       (> *neraser-rows* row)
       (> *neraser-columns* column)
       (nthelt (neraser-cell-index row column) *neraser-board*)))

(defun neraser-cell-index (row column)
  "Get linear index on the game board for given board coordinates"
  (+ column (* row *neraser-columns*)))

(define-derived-mode neraser-mode special-mode "Number eraser game"
  "Game of number collection mode

Use C-n, C-f to switch between syncs, keyboard arrows to move them"
  (define-key neraser-mode-map (kbd "<up>") 'neraser-move-up)
  (define-key neraser-mode-map (kbd "C-p") 'neraser-move-up)
  (define-key neraser-mode-map (kbd "<left>") 'neraser-move-left)
  (define-key neraser-mode-map (kbd "C-b") 'neraser-move-left)
  (define-key neraser-mode-map (kbd "<down>") 'neraser-move-down)
  (define-key neraser-mode-map (kbd "C-n") 'neraser-move-down)
  (define-key neraser-mode-map (kbd "<right>") 'neraser-move-right)
  (define-key neraser-mode-map (kbd "C-f") 'neraser-move-right)
  (define-key neraser-mode-map (kbd "M-p") 'neraser-go-prev-sync)
  (define-key neraser-mode-map (kbd "M-n") 'neraser-go-next-sync)
  (define-key neraser-mode-map (kbd "r") 'neraser-reset))

(defun neraser-game (n)
  "Start playing number eraser game on board number n (0 by default)"
  (interactive "P")
  (switch-to-buffer "*Number eraser*")
  (neraser-mode)
  (neraser-init (if n (prefix-numeric-value n) 0)))

(defun neraser-init (n)
  "Prepare to start the game on nth map"
  ;; prepare board by loading map
  (or (neraser-load-map (nth n *neraser-default-maps*))
      (print "Failed to load map ((( \n"))
  ;; reset step counter to zero
  (setf *neraser-step-counter* 0)
  ;; select first sync
  (setf *neraser-sync-row* 0)
  (setf *neraser-sync-column* 0)
  (neraser-next-sync)
  ;; display board
  (neraser-draw-board)
  (neraser-goto-cell *neraser-sync-row* *neraser-sync-column*))

(defmacro deconstruct-index (index row column)
  "Set provided row and column variables to position on board referred by index"
  (let ((index-inner (gensym)))
    `(let ((,index-inner ,index))
       (cond
         (,index-inner
          (setf ,row (/ ,index-inner *neraser-columns*))
          (setf ,column (% ,index-inner *neraser-columns*)))
         (t
          (setf ,row *neraser-rows*)
          (setf ,column *neraser-columns*))))))

(defun eq-left2 (left right)
  "Helper funtion that tests if right is equal to any of 2 elements in left"
  (cl-destructuring-bind (l1 l2) left
    (or (eq l1 right)
        (eq l2 right))))

(defun neraser-next-sync ()
  "Find next sync and set current board position to it"
  (let* ((current-sync-index (neraser-cell-index *neraser-sync-row* *neraser-sync-column*))
         (next-sync-index (position '(*sync* *c-sync*) *neraser-board*
                                    :key 'car
                                    :test 'eq-left2
                                    :start (1+ current-sync-index)
                                    :end (neraser-cell-index (1- *neraser-rows*) *neraser-columns*))))
    (if next-sync-index
        (deconstruct-index next-sync-index *neraser-sync-row* *neraser-sync-column*)
      (deconstruct-index (position '(*sync* *c-sync*) *neraser-board*
                                   :key 'car
                                   :test 'eq-left2
                                   :start 0
                                   :end (1+ current-sync-index))
                         *neraser-sync-row* *neraser-sync-column*))))

(defun neraser-prev-sync ()
  "Find prev sync and set current board position to it"
  (let* ((current-sync-index (neraser-cell-index *neraser-sync-row* *neraser-sync-column*))
         (prev-sync-index (position '(*sync* *c-sync*) *neraser-board*
                                    :key 'car
                                    :test 'eq-left2
                                    :start 0
                                    :end current-sync-index
                                    :from-end)))
    (if prev-sync-index
        (deconstruct-index prev-sync-index *neraser-sync-row* *neraser-sync-column*)
      (deconstruct-index (position '(*sync* *c-sync*) *neraser-board*
                                   :key car
                                   :test 'eq-left2
                                   :start current-sync-index
                                   :end (neraser-cell-index (1- *neraser-rows*) *neraser-columns*)
                                   :from-end)
                         *neraser-sync-row* *neraser-sync-column*))))

(defun neraser-left-angle (row col)
  "Symbol that forms left upper angle of a given cell"
  ?+)

(defun neraser-upper-border (row col)
  "Symbol that forms upper border of a given cell"
  ?=)

(defun neraser-upper-boundary (row col)
  "Make a string for the upper left angle and top border of the displayed board cell"
  (format "%c%s" (neraser-left-angle row col) (make-string *neraser-cell-width* (neraser-upper-border row col))))

(defun neraser-upper-angle (row)
  "Symbol that forms right upper angle of a given row"
  "+\n")

(defun neraser-right-boundary (row)
  "Symbol that forms right boundary of a given row"
  ?|)

(defun neraser-left-boundary (row col)
  "Symbol that forms left boundary of a given cell"
  ?|)

(defun neraser-empty-space (row col)
  "Symbol that forms empty space of a given cell"
  (make-string *neraser-cell-width* ?\s))

(defun neraser-right-boundary (row)
  "Symbol that forms right boundary of a given row"
  "|\n")

(defun neraser-left-lower-angle (row col)
  "Symbol that forms left lower angle of a given cell"
  ?+)

(defun neraser-lower-border (row col)
  "Symbol that forms lower border of a given cell"
  ?=)

(defun neraser-lower-boundary (row col)
  "Make a string for the upper left angle and top border of the displayed board cell"
  (format "%c%s" (neraser-left-lower-angle row col) (make-string *neraser-cell-width* (neraser-lower-border row col))))

(defun neraser-lower-angle (row)
  "Symbol that forms right lower angle of a given row"
  "+\n")

(defun neraser-cell-content (row col)
  "Form string based on cell's contents"
  (let* ((content (neraser-get-cell row col))
         (type (car content)))
    (cond
     ((eq type '*cell*)
      (format "%2d/%1d" (nthelt 0 (cdr content)) (nthelt 1 (cdr content))))
     ((eq type '*sync*)
      (if (eq 0 (cdr content)) ; test if sync cannot be moved anymore
          "*VV*"
        (replace-regexp-in-string " " "*" (format "%3d " (cdr content)))))
     ((eq type '*c-sync*)
      (if (eq 0 (nthelt 0 (cdr content)))
          "*VV*"
        (replace-regexp-in-string " " "*" (format "%3d " (nthelt 0 (cdr content))))))
     ((or t (null content))
      (neraser-empty-space row col)))))

(defun neraser-draw-board ()
  "Display board in the game buffer"
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dotimes (row *neraser-rows*)
      ;; todo: use pretty pseudographics there
      (dotimes (col *neraser-columns*)
        (insert (neraser-upper-boundary row col)))
      (insert (neraser-upper-angle row))
      (let ((boundary (neraser-right-boundary row)))
        ;; empty space above cell
        (dotimes (col *neraser-columns*)
          (insert (neraser-left-boundary row col))
          (insert (neraser-empty-space row col)))
        (insert boundary)
        ;; number in the cell
        (dotimes (col *neraser-columns*)
          (insert (neraser-left-boundary row col))
          (insert (neraser-cell-content row col)))
        (insert boundary)
        ;; empty space under cell
        (dotimes (col *neraser-columns*)
          (insert (neraser-left-boundary row col))
          (insert (neraser-empty-space row col)))
        (insert boundary)))
    (dotimes (col *neraser-columns*)
      (insert (neraser-lower-boundary (1- *neraser-rows*) col)))
    (insert (neraser-lower-angle (1- *neraser-rows*)))))

(defun neraser-cursor-to-board-cell (row column)
  "Move cursor to the board cell with known coordinates"
  (let ((line (* 4 row))
        (char (* (1+ *neraser-cell-width*) column)))
    (move-to-window-line (1- line))
    (forward-char (- char 2))))

(defun neraser-goto-cell (row column)
  "Put cursor into cell on the game board"
  ;; beginning of the buffer
  (goto-char (point-min))
  ;; 2 lines lower then the upper border of a cell
  (forward-line (+ (* *neraser-cell-height* row) 2))
  ;; we place cursor on the first position of the middle line of a cell
  (forward-char (1+ (* (1+ *neraser-cell-width*) column))))

(defun neraser-go-next-sync ()
  "Select next sync and move cursor there"
  (interactive)
  (neraser-next-sync)
  (neraser-goto-cell *neraser-sync-row* *neraser-sync-column*))

(defun neraser-go-prev-sync ()
  "Select prev sync and move cursor there"
  (interactive)
  (neraser-prev-sync)
  (neraser-goto-cell *neraser-sync-row* *neraser-sync-column*))

(defun neraser-reset ()
  "Reset game board to initial state"
  ;; reset steps counter
  (interactive)
  (setq *neraser-step-counter* 0)
  (setf *neraser-board* (copy-tree *neraser-initial-board* t))
  (neraser-next-sync)
  (neraser-draw-board)
  (neraser-goto-cell *neraser-sync-row* *neraser-sync-column*))

(defun neraser-generic-move (row-shift col-shift dirstring)
  "Change current sync row to (row-shift row), column to (col-shift col) in the direction described by dirstring"
  (let ((current-sync (neraser-get-cell *neraser-sync-row* *neraser-sync-column*))
        (future-sync (neraser-get-cell (funcall row-shift *neraser-sync-row*) (funcall col-shift *neraser-sync-column*))))
    (if (and current-sync
             future-sync
             (or (eq (car current-sync) '*sync*)
                 (eq (car current-sync) '*c-sync*))
             (eq (car future-sync) '*cell*)
             (> (nthelt 1 (cdr future-sync)) 0))
        (progn
          ;(debug)
          (message (format "Moving current sync %s" dirstring))
          (cl-destructuring-bind 
            (current-val current-cell current-count future-cell future-count)
            (append (if (eq (car current-sync) '*sync*)
                        (list (cdr current-sync) 0 0)
                      (cdr current-sync))
                    (cdr future-sync) nil)
            (setcdr future-sync `[,(- current-val future-cell) ,future-cell ,(1- future-count)])
            (setcdr current-sync `[,current-cell ,current-count])
            (setcar future-sync '*c-sync*)
            (setcar current-sync '*cell*))
          (setq *neraser-sync-row* (funcall row-shift *neraser-sync-row*))
          (setq *neraser-sync-column* (funcall col-shift *neraser-sync-column*))
          (setq *neraser-step-counter* (1+ *neraser-step-counter*))
          (neraser-check-victory)
          (neraser-draw-board)
          (neraser-goto-cell *neraser-sync-row* *neraser-sync-column*))
      (message (format "Cannot move current sync %s" dirstring)))))

(defun neraser-move-up ()
  "Make a turn by moving sync up"
  (interactive)
  (neraser-generic-move '1- 'identity "up"))

(defun neraser-move-down ()
  "Make a turn by moving sync down"
  (interactive)
  (neraser-generic-move '1+ 'identity "down"))

(defun neraser-move-left ()
  "Make a turn by moving sync left"
  (interactive)
  (neraser-generic-move 'identity '1- "left"))

(defun neraser-move-right ()
  "Make a turn by moving sync right"
  (interactive)
  (neraser-generic-move 'identity '1+ "right"))

(defun neraser-check-victory ()
  "Check if victory or loss condition (weak) has been reached"
  (cl-labels ((victory-key (board-elem)
                           (cond
                            ((eq (car board-elem) '*c-sync*)
                             (nthelt 0 (cdr board-elem)))
                            ((eq (car board-elem) '*sync*)
                             (cdr board-elem))
                            (t 0))))
    (let ((victory-index (position 0 *neraser-board*
                                   :key #'victory-key
                                   :test-not 'eq)))
      (if (null victory-index)
          (message "Congratulations!")))))


  
