
; problem 1

(defun my-last (ls)
  (cond
    ((eq (length ls) 1) (car ls))
    (t (my-last (cdr ls)))
    )
  )

; problem 2

(defun my-but-last (ls)
  (cond
    ((eq (length ls) 2) ls)
    (t (my-but-last (cdr ls)))
    )
  )

; problem 3

(defun element-at (ls ind)
  (cond
    ((eq ind 1) (car ls))
    (t (element-at (cdr ls) (- ind 1)))
    )
  )

; problem 4

(defun num-elements (ls)
  (cond
    ((eq ls nil) 0)
    (t (+ 1 (num-elements (cdr ls))))
    )
  )

; problem 5

(defun reverse-ls-helper (ls acc)
  (cond
    ((eq ls nil) acc)
    (t (reverse-ls-helper (cdr ls) (cons (car ls) acc)))
    )
  )

(defun reverse-ls (ls)
  (reverse-ls-helper ls nil))

