
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


; problem 12

(defun decode-one-helper (x n acc)
  (cond
    ((eq n 0) acc)
    (t (decode-one-helper x (- n 1) (cons x acc)))
    )
  )

(defun decode-one (x n)
  (decode-one-helper x n nil))


(defun decode (ls)
  (mapcar #'(lambda (p)
              (cond
                ((listp p) (decode-one (element-at p 2) (element-at p 1)))
                (t (decode-one p 1)))) ls))

; problem 13


(defun encode-direct-helper (ls cur n)
  (cond
    ((equal ls nil) (list (list n cur)))
    ((equal (car ls) cur) (encode-direct-helper (cdr ls) cur (+ n 1)))
    ((> n 1) (cons (list n cur) (encode-direct-helper (cdr ls) (car ls) 1)))
    (t (cons cur (encode-direct-helper (cdr ls) (car ls) 1)))
    ))

(defun encode-direct (ls)
  (cond
    ((equal ls nil) nil)
    (t (encode-direct-helper (cdr ls) (car ls) 1))))

; problem 14

(defun dupli (ls)
  (cond
    ((equal ls nil) nil)
    (t (cons (car ls) (cons (car ls) (dupli (cdr ls)))))))

; problem 15

(defun repli (ls n)
  (cond
    ((equal ls nil) nil)
    (t (append (decode-one (car ls) n) (repli (cdr ls) n)))))


; problem 16

(defun drop-helper (ls n cur)
  (cond
    ((equal ls nil) nil)
    ((equal n cur) (drop-helper (cdr ls) n 1))
    (t (cons (car ls) (drop-helper (cdr ls) n (+ 1 cur))))))

(defun drop (ls n)
  (drop-helper ls n 1))

; problem 17

(defun split (ls n)
  (cond
    ((equal n 0) (list nil ls))
    (t (let ((res (split (cdr ls) (- n 1))))
         (cons (cons (car ls) (car res)) (cdr res))))))


; problem 18

(defun slice (ls from to)
  (cond
    ((equal to 0) nil)
    ((<= from 1) (cons (car ls) (slice (cdr ls) (- from 1) (- to 1))))
    (t (slice (cdr ls) (- from 1) (- to 1)))))

; problem 19

(defun rotate (ls n)
  (cond
    ((< n 1) (rotate ls (+ n (length ls))))
    ((> n (length ls)) (rotate ls (- n (length ls))))
    (t (let ((splitlist (split ls n)))
         (let
             ((front (element-at splitlist 1))
              (back (element-at splitlist 2)))
             (append back front))))))


; problem 20

(defun remove-at (ls n)
  (cond
    ((equal n 1) (cdr ls))
    (t (cons (car ls) (remove-at (cdr ls) (- n 1))))))

; problem 21

(defun insert-at (elem ls ind)
  (cond
    ((equal ind 1) (cons elem ls))
    (t (cons (car ls) (insert-at elem (cdr ls) (- ind 1))))))

; problem 22

(defun range (from to)
  (cond
    ((> from to) nil)
    (t (cons from (range (+ from 1) to)))))

; problem 23

(defun rnd-select (ls n)
  (cond
    ((equal ls nil) nil)
    ((< (random (length ls)) n) (cons (car ls) (rnd-select (cdr ls) (- n 1))))
    (t (rnd-select (cdr ls) n))))

; problem 24

(defun lotto-select (n m)
  (rnd-select (range 1 m) n))

; problem 25

(defun rnd-permu-helper (ls acc inds)
  (cond
    ((eq (length inds) 0) acc)
    (t (let ((indinds (+ (random (length inds)) 1)))
             (rnd-permu-helper ls (cons (element-at ls (element-at inds indinds)) acc) (remove-at inds indinds))))))

; 

(defun rnd-permu (ls)
  (rnd-permu-helper ls nil (range 1 (length ls))))
