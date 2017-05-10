;;; contains lisp solutions for little programming problems


(defun equal_elements (list1 list2)
  "From two sorted (ascending order) lists return list of the same elements"
  (let ((first1 (car list1))
        (rest1 (cdr list1))
        (first2 (car list2))
        (rest2 (cdr list2)))
    (cond
     ((not (and first1 first2))
      '())
     ((= first1 first2)
      (cons first1 (equal_elements rest1 rest2)))
     ((> first1 first2)
      (equal_elements list1 rest2))
     (t
      (equal_elements rest1 list2)))))

(defun add-to-hash (c ht)
  "Add an instance of character C to hash table HT"
  (puthash c (1+ (gethash c ht 0)) ht))

(defun remove-from-hash (c ht &optional try)
  "Remove an instance of character C from hash table HT
Return nil if no instances of C remain in HT"
  (let ((old (gethash c ht 0)))
    (if (and try (< 2 old))
        nil
      (if (> 1 (puthash c (1- (gethash c ht 0)) ht))
          (remhash c ht)
        t))))

(defun find-min-substring (sstr)
  "Find minimal substring that contains all the characters in a given string"
  ;; get all characters
  (let* ((all-chars (make-hash-table))
         (slen (length sstr))
         (fcnt (progn
                 (mapc (lambda (c) (add-to-hash c all-chars)) sstr)
                 (hash-table-count all-chars)))
         (beg 0) (end fcnt)
         (res sstr))
    (if (= end slen)
        res
      (let* ((cand (substring sstr beg end))
             (cand-chars (make-hash-table))
             (ccnt (progn
                     (mapc (lambda (c) (add-to-hash c cand-chars)) cand)
                     (hash-table-count cand-chars))))
        ;; find first candidate, that is a substring with all the characters
        (while (< ccnt fcnt)
          (add-to-hash (aref sstr end) cand-chars)
          (setq end (1+ end))
          (setq ccnt (hash-table-count cand-chars)))
        (setq cand (substring sstr beg end))
        ;; shorten it as much as possible
        (while (remove-from-hash (aref sstr beg) cand-chars t)
          (setq beg (1+ beg)))
        (setq cand (substring sstr beg end))
        (setq res cand)
        ;; check other variants
        (while (< end slen)
          ;; advance both ends
          (add-to-hash (aref sstr end) cand-chars)
          (setq end (1+ end))
          (remove-from-hash (aref sstr beg) cand-chars)
          (setq beg (1+ beg))
          (setq ccnt (hash-table-count cand-chars))
          ;; another candidate is found
          (when (= ccnt fcnt)
            ;; only change candidate if it was shortened
            (while (remove-from-hash (aref sstr beg) cand-chars t)
              (setq beg (1+ beg))
              (setq cand (substring sstr beg end)))
            (setq res cand)))
        res))))
