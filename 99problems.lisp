
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

; problem 6

(defun palindrome (ls)
  (equal ls (reverse-ls ls)))

; problem 7

(defun my-flatten (ls)
  (cond
      ((equal ls nil) nil)
      ((listp (car ls)) (append (my-flatten (car ls)) (my-flatten (cdr ls))))
      (t (cons (car ls) (my-flatten (cdr ls))))
      )
  )


; problem 8

(defun compress (ls)
  (cond
    ((<= (length ls) 1)  ls)
    ((equal (car ls) (car (cdr ls))) (compress (cdr ls)))
    (t (cons (car ls) (compress (cdr ls))))
    )
  )


; problem 9
(defun pack-helper (ls acc)
  (cond
    ((equal ls nil) (list acc))
    ((equal acc nil) (pack-helper (cdr ls) (list (car ls))))
    ((equal (car ls) (car acc)) (pack-helper (cdr ls) (cons (car ls) acc)))
    (t (cons acc (pack-helper ls nil)))
    )
  )

(defun pack (ls)
  (pack-helper ls nil))


; problem 10

(defun encode (ls)
  (mapcar #'(lambda (x) (list (length x) (car x))) (pack ls)))


; problem 11

(defun encode-modified (ls)
  (mapcar #'(lambda (p)
              (cond
                ((eq (car p) 1) (car (cdr p)))
                (t p))) (encode ls)))

