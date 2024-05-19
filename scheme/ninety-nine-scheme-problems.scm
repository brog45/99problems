#!r6rs

;; http://community.schemewiki.org/?ninety-nine-scheme-problems

(import (rnrs (6))
;        (rnrs lists (6))
;        (rnrs base (6))
;        (rnrs io simple (6))
        (srfi :64)
        (srfi :27))

;; utility functions for converting #lang racket to #lang r6rs

(define (swap-args f)
  (lambda (a b)
    (f b a)))
(define (foldl reducer accumulater ls)
  (fold-left (swap-args reducer) accumulater ls))
(define foldr fold-right)
(define empty? null?)
(define modulo mod)
(define random random-integer)

(define (range a b)
  (if (>= a b)
      '()
      (cons a (range (+ a 1) b))))

(define (repeat n f)
  (when (> n 0)
    (f)
    (repeat (- n 1) f)))


;; 1. (*) Find the last box of a list. S-99-01

(define (last ls)
  (if (null? (cdr ls))
   (car ls)
   (last (cdr ls))))

(test-begin "S-99-01")
(test-equal 1 (last '(1)))
(test-equal 3 (last '(1 2 3)))
(test-equal 'c (last '(a b c)))
(test-error (last '()))
(test-end "S-99-01")

;; 2. (*) Find the last but one box of a list. S-99-02

(define (last-but-one ls)
  (if (null? (cddr ls))
      (car ls)
      (last-but-one (cdr ls))))

(test-begin "S-99-02")
(test-error (last-but-one '()))
(test-error (last-but-one '(a)))
(test-equal 2 (last-but-one '(1 2 3)))
(test-equal 1 (last-but-one '(1 2)))
(test-end "S-99-02")

;; 3. (*) Find the K'th element of a list. S-99-03

(define (kth k ls)
  (if (= k 1)
      (car ls)
      (kth (- k 1) (cdr ls))))

(test-begin "S-99-03")
(test-error (kth 0 '(a)))
(test-error (kth -1 '(a)))
(test-error (kth 0 '()))
(test-error (kth 1 '()))
(test-eq 'a (kth 1 '(a)))
(test-eq 'a (kth 1 '(a b)))
(test-eq 'b (kth 2 '(a b)))
(test-end "S-99-03")

;; 4. (*) Find the number of elements of a list. S-99-04

(define (len ls)
  (define (f _ n) (+ 1 n))
  (foldl f 0 ls))

(test-begin "S-99-04")
(test-eq 0 (len '()))
(test-eq 1 (len '(17)))
(test-eq 1 (len '(a)))
(test-eq 2 (len '(a b)))
(test-end "S-99-04")

;; 5. (*) Reverse a list. S-99-05

(define (rev ls)
  (foldl cons '() ls))

(test-begin "S-99-05")
(test-equal '() (rev '()))
(test-equal '(a) (rev '(a)))
(test-equal '(b a) (rev '(a b)))
(test-end "S-99-05")

;; 6. (*) Find out whether a list is a palindrome. S-99-06

(define (pal? ls)
  (equal? ls (rev ls)))

(test-begin "S-99-06")
(test-assert (pal? '()))
(test-assert (pal? '(a)))
(test-assert (pal? '(a b a)))
(test-assert (not (pal? '(a b c))))
(test-end "S-99-06")

;;    (**) Flatten a nested list structure. S-99-07

(define (flat ls)
    (define (f item acc)
      (if (pair? item)
          (foldr f acc (flat item))
          (cons item acc)))
    (foldr f '() ls))

(test-begin "S-99-07")
(test-equal '() (flat '()))
(test-equal '(a) (flat '(a)))
(test-equal '(a b c) (flat '(a b c)))
(test-equal '(a b c) (flat '((a b) c)))
(test-equal '(a b c) (flat '((((a)) b) (c))))
(test-end "S-99-07")

;;    (**) Eliminate consecutive duplicates of list elements. S-99-08

(define (dedup ls)
  (define (f item acc)
    (if (and (pair? acc)
             (equal? item (car acc)))
        acc
        (cons item acc)))
  (foldr f '() ls))

(test-begin "S-99-08")
(test-equal '() (dedup '()))
(test-equal '(a b c) (dedup '(a b c)))
(test-equal '(a) (dedup '(a a)))
(test-equal '(a b c) (dedup '(a b b c)))
(test-equal '(a b c) (dedup '(a a a b b b c c c)))
(test-equal '((a b) a b) (dedup '((a b) (a b) a b)))
(test-equal '((a) a) (dedup '((a) (a) (a) a a a)))
(test-end "S-99-08")

;;    (**) Pack consecutive duplicates of list elements into sublists. S-99-09

(define (pack l)
  (define (f item acc)
    (cond
      ((null? acc) (cons (list item) '()))
      ((eq? item (caar acc))
       (let [(this-group (car acc))
             (prev-groups (cdr acc))]
         (cons (cons item this-group) prev-groups)))
      (else (cons (list item) acc))))
  (foldr f '() l))

(test-begin "S-99-09")
(test-equal '() (pack '()))
(test-equal '((1)) (pack '(1)))
(test-equal '((1 1)) (pack '(1 1)))
(test-equal '((1 1) (2)) (pack '(1 1 2)))
(test-equal '((1) (2)) (pack '(1 2)))
(test-equal '((a a a a) (b) (c c) (a a) (d) (e e e e))
            (pack '(a a a a b c c a a d e e e e)))
(test-end "S-99-09")

;;    (*) Run-length encoding of a list. S-99-10
;;
;; Use the result of problem S-99-09 to implement the so-called run-length
;; encoding data compression method. Consecutive duplicates of elements are
;; encoded as lists (N E) where N is the number of duplicates of the element E.
;; 
;; Example:
;; 
;;  (encode '(a a a a b c c a a d e e e e)) 
;;  ; => ((4 a) (1 b) (2 c) (2 a) (1 d) (4 e)) 
;; 

(define (encode l)
  (define (f l) (list (length l) (car l)))
  (map f (pack l)))

(test-begin "S-99-10")
(test-equal '() (encode '()))
(test-equal '((1 a)) (encode '(a)))
(test-equal '((4 a) (1 b) (2 c) (2 a) (1 d) (4 e))
            (encode '(a a a a b c c a a d e e e e)))
(test-end "S-99-10")

;;    (*) Modified run-length encoding. S-99-11
;;
;; Modify the result of problem S-99-10 in such a way that if an element has no
;; duplicates it is simply copied into the result list. Only elements with
;; duplicates are transferred as (N E) lists.
;; 
;; Example:
;; 
;;  (encode-modified '(a a a a b c c a a d e e e e)) 
;;  ; => ((4 a) b (2 c) (2 a) d (4 e)) 
;; 

(define (encode-modified l)
  (define (f l)
    (if (= (car l) 1)
        (cadr l)
        l))
  (map f (encode l)))

(test-begin "S-99-11")
(test-equal '() (encode-modified '()))
(test-equal '(a) (encode-modified '(a)))
(test-equal '((4 a) b (2 c) (2 a) d (4 e))
            (encode-modified '(a a a a b c c a a d e e e e)))
(test-end "S-99-11")

;;    (**) Decode a run-length encoded list. S-99-12
;;
;; Given a run-length code list generated as specified in problem S-99-11.
;; Construct its uncompressed version.

(define (decode-rle-tuple t)
  (let loop [(n (car t))
             (a (cadr t))
             (acc '())]
    (if (= 1 n)
        (cons a acc)
        (loop (- n 1) a (cons a acc)))))

(define (decode-rle-item item)
  (if (pair? item)
      (decode-rle-tuple item)
      item))

(define (decode l)
  (flat (map decode-rle-item l)))

(test-begin "S-99-12")
(test-equal '() (decode '()))
(test-equal '(a a a a b c c a a d e e e e)
            (decode '((4 a) b (2 c) (2 a) d (4 e))))
(test-end "S-99-12")

;;    (**) Run-length encoding of a list (direct solution). S-99-13
;;
;; Implement the so-called run-length encoding data compression method directly.
;; I.e. don't explicitly create the sublists containing the duplicates, as in
;; problem S-99-09, but only count them. As in problem S-99-11, simplify the
;; result list by replacing the singleton lists (1 X) by X.

(define (encode-direct l)
  (let loop [(n 0) (p #f) (l l)]
    (cond
      ((empty? l)
       (cond
         ((= n 0) '())
         ((= n 1) (list p))
         (else
          (cons (list n p) '()))))
      ((= n 0) (loop 1 (car l) (cdr l)))
      ((eq? p (car l)) (loop (+ 1 n) p (cdr l)))
      ((= n 1) (cons p (loop 0 #f l)))
      (else (cons (list n p) (loop 0 #f l))))))

(test-begin "S-99-13")
(test-equal '((4 a) b (2 c) (2 a) d (4 e))
            (encode-direct '(a a a a b c c a a d e e e e)))
(test-equal '() (encode-direct '()))
(test-equal '(a) (encode-direct '(a)))
(test-equal '(a b c) (encode-direct '(a b c)))
(test-end "S-99-13")

;;    (*) Duplicate the elements of a list. S-99-14
;; 
;;  (dupli '(a b c c d)) 
;;  ; => (a a b b c c c c d d) 

(define (dupli l)
  (define (f item acc) (cons item (cons item acc)))
  (foldr f '() l))

(test-begin "S-99-14")
(test-equal '(a a b b c c c c d d)
            (dupli '(a b c c d)))
(test-equal '() (dupli '()))
(test-equal '(a a) (dupli '(a)))
(test-equal '(a a a a) (dupli '(a a)))
(test-end "S-99-14") 

;;    (**) Replicate the elements of a list a given number of times. S-99-15
;; 
;;  (repli '(a b c) 3) 
;;  ; => (a a a b b b c c c) 

(define (repli l n)
  (define (f item acc)
    (let loop [(n n) (acc acc)]
      (if (> n 0)
          (loop (- n 1) (cons item acc))
          acc)))
  (foldr f '() l))

(test-begin "S-99-15")
(test-equal '(a a a b b b c c c)
            (repli '(a b c) 3))
(test-equal '() (repli '() 3))
(test-equal '() (repli '(a b c) 0))
(test-equal '(a b c) (repli '(a b c) 1))
(test-end "S-99-15") 

;;    (**) Drop every N'th element from a list. S-99-16

(define (drop-every-nth n l)
  (let loop [(l l) (p 1)]
    (cond
      [(empty? l) l]
      [(= 0 (modulo p n)) (loop (cdr l) 1)]
      [else (cons (car l) (loop (cdr l) (+ 1 p)))])))

(test-begin "S-99-16")
(test-equal '() (drop-every-nth 1 '()))
(test-equal '() (drop-every-nth 2 '()))
(test-equal '() (drop-every-nth 1 '(a b c)))
(test-equal '(a) (drop-every-nth 2 '(a)))
(test-equal '(a) (drop-every-nth 2 '(a b)))
(test-equal '(a c) (drop-every-nth 2 '(a b c)))
(test-equal '(a b d e) (drop-every-nth 3 '(a b c d e f)))
(test-end "S-99-16")

;;    (*) Split a list into two parts; the length of the first part is given. S-99-17
;; Do not use any predefined predicates.
;;
;; (split '(a b c d e f g h i k) 3) 
;;  => ((a b c) (d e f g h i k)) 

(define (take-n n l)
  (cond
    [(= 0 n) '()]
    [(null? l) l]
    [else (cons (car l) (take-n (- n 1) (cdr l)))] ))

(define (skip-n n l)
  (cond
    [(= 0 n) l]
    [(null? l) l]
    [else (skip-n (- n 1) (cdr l))] ))

(define (split l n)
  (list
   (take-n n l)
   (skip-n n l)))

(test-begin "S-99-17")
(test-equal '((a b c) (d e f g h i k))
            (split '(a b c d e f g h i k) 3))
(test-end "S-99-17")

;;    (**) Extract a slice from a list. S-99-18
;; 
;; Given two indices, I and K, the slice is the list containing the elements
;; between the I'th and K'th element of the original list (both limits included).
;; Start counting the elements with 1.
;; 
;; Example:
;; 
;;  (slice '(a b c d e f g h i k) 3 7)
;;  ; => (c d e f g)

(define (slice l start stop)
  (when (and (> start 0) (>= stop start))
    (cond
      [(empty? l) '()]
      [(> start 1) (slice (cdr l) (- start 1) (- stop 1))]
      [(= start stop) (list (car l))]
      [else (cons (car l) (slice (cdr l) start (- stop 1)))])))

(test-begin "S-99-18")
(test-equal '(c d e f g)
            (slice '(a b c d e f g h i k) 3 7))
(test-end "S-99-18")

;;    (**) Rotate a list N places to the left. S-99-19
;; 
;; Examples:
;; 
;;  (rotate '(a b c d e f g h) 3) 
;;  ; => (d e f g h a b c) 
;;   
;;  (rotate '(a b c d e f g h) -2) 
;;  ; => (g h a b c d e f) 
;; 
;; Hint: Use the predefined functions length and append, as well as the result
;; of S-99-17. 

(define (rotate l n)
  (cond
    [(empty? l) l]
    [(= n 0) l]
    [(> n 0) (let* [(splitted (split l n))
                    (left (car splitted))
                    (right (cadr splitted))]
               (append right left))]
    [else (rotate l (+ (length l) n))]))

(test-begin "S-99-19")
(test-equal '()
            (rotate '() 1))
(test-equal '(a b c)
            (rotate '(a b c) 0))
(test-equal '(d e f g h a b c) 
            (rotate '(a b c d e f g h) 3))
(test-equal '(g h a b c d e f) 
            (rotate '(a b c d e f g h) -2))
(test-end "S-99-19")

;;    (*) Remove the K'th element from a list. S-99-20
;; Example:
;; 
;; (remove-at '(a b c d) 2) 
;;  ; => (a c d) 

(define (remove-at l n)
  (cond
    [(null? l) '()]
    [(< n 0) (reverse (remove-at (reverse l) (- 0 n)))]
    [(= n 0) l]
    [(= n 1) (cdr l)]
    [else (cons (car l) (remove-at (cdr l) (- n 1)))]))

(test-begin "S-99-20")
(test-equal '()
            (remove-at '() 3))
(test-equal '(a c d)
            (remove-at '(a b c d) 2))
(test-equal '(a b d)
            (remove-at '(a b c d) -2))
(test-equal '(a b c d)
            (remove-at '(a b c d) 0))
(test-equal '(a b c d)
            (remove-at '(a b c d) 1000))
(test-end "S-99-20")

;;    (*) Insert an element at a given position into a list. S-99-21

(define (insert-at x xs n)
  (cond
    [(< n 0) (reverse (insert-at x (reverse xs) (- 0 n)))]
    [(= n 0) xs]
    [(= n 1) (cons x xs)]
    [(empty? xs) '()]
    [else (cons (car xs) (insert-at x (cdr xs) (- n 1)))]))

(test-begin "S-99-21")
(test-equal '(a alfa b c d)
            (insert-at 'alfa '(a b c d) 2))
(test-equal '(a b c alfa d)
            (insert-at 'alfa '(a b c d) -2))
(test-equal '(a b c d)
            (insert-at 'alfa '(a b c d) 0))
(test-equal '()
            (insert-at 'alfa '() 2))
(test-equal '(alfa)
            (insert-at 'alfa '() 1))
(test-equal '(a b c d)
            (insert-at 'alfa '(a b c d) 1000))
(test-end "S-99-21")

;;    (*) Create a list containing all integers within a given range. S-99-22

(define (from-to i k)
  (cond
    [(= i k) (list i)]
    [(< i k) (cons i (from-to (+ i 1) k))]
    [(> i k) (reverse (from-to k i))]))

(test-begin "S-99-22")
(test-equal '(1) (from-to 1 1))
(test-equal '(1 2 3) (from-to 1 3))
(test-equal '(3 2 1) (from-to 3 1))
(test-end "S-99-22")

;;    (**) Extract a given number of randomly selected elements from a list. S-99-23
;; Example:
;;   (rnd-select '(a b c d e f g h) 3)
;;   => (e d a)

(define (random-select-no xs k)
  (cond
    [(= k 0) '()]
    [(empty? xs) '()]
    [else
     (let ([n (+ 1 (random (length xs)))])
       (cons (kth n xs)
             (random-select-no (remove-at xs n) (- k 1))))]))

(test-begin "S-99-23")
(test-equal '() (random-select-no '() 3))
(test-equal '() (random-select-no '(a b) 0))
(test-equal 1 (length (random-select-no '(a b) 1)))
(repeat 100
        (lambda ()
          (test-assert
           (member (random-select-no '(a b) 2)
                   '((a b) (b a))))))
(test-assert (<= (length
                  (dedup
                   (map (lambda _ (random-select-no '(a b) 2))
                        (range 0 20))))
                 20))
(test-end)

;;    (*) Lotto: Draw N different random numbers from the set 1..M. S-99-24

(define (pick-numbers m n)
  (let [(num-set (from-to 1 m))]
    (random-select-no num-set n)))

(test-begin "S-99-24")
(test-equal '() (pick-numbers 1 0))
(test-equal '(1) (pick-numbers 1 1))
(repeat 100
        (lambda ()
          (test-assert
           (member (pick-numbers 3 2)
                   '((1 2) (1 3) (2 1) (2 3) (3 1) (3 2))))))
(test-end)

;;    (*) Generate a random permutation of the elements of a list. S-99-25

(define (random-permutation xs)
  (let [(n (length xs))]
    (random-select-no xs n)))

(test-begin "S-99-25")
(test-equal '() (random-permutation '()))
(test-equal '(1) (random-permutation '(1)))
(repeat 100
        (lambda ()
          (test-assert
           (member (random-permutation '(1 2))
                   '((1 2) (2 1))))))
(test-end)

;;    (**) Generate the combinations of K distinct objects chosen from the N elements of a list S-99-26
;; Example:
;;  (combination 3 '(a b c d)) 
;;  ; => ((b c d) (a c d) (a b d) (a b c)) 

(define (combination k xs)
  (cond
    [(< k 1) #f]
    [(empty? xs) '()]
    [(= k 1) (map list xs)]
    [else (append
              (map (lambda (l) (cons (car xs) l))
               (combination (- k 1) (cdr xs)))
              (combination k (cdr xs)))]))

(test-begin "S-99-26")
(test-equal '() (combination 1 '()))
(test-equal '() (combination 2 '()))
(test-equal '((a) (b) (c)) (combination 1 '(a b c)))
(test-equal '((a b) (a c) (b c)) (combination 2 '(a b c)))
(test-equal '((a b c) (a b d) (a c d) (b c d))
            (combination 3 '(a b c d)))
(test-equal '((a b c d))
            (combination 4 '(a b c d)))
(test-end)

;;    (**) Group the elements of a set into disjoint subsets. S-99-27
;; a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities and returns them in a list.
;;
;;    Example:
;;
;;     (group3 '(aldo beat carla david evi flip gary hugo ida)) 
;;     ; => ( ( (ALDO BEAT) (CARLA DAVID EVI) (FLIP GARY HUGO IDA) ) 
;;            ... )
;;
;; b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.
;;
;;    Example:
;;
;;     (group '(aldo beat carla david evi flip gary hugo ida) '(2 2 5)) 
;;     ; => ( ( (ALDO BEAT) (CARLA DAVID) (EVI FLIP GARY HUGO IDA) ) 
;;          ... ) 


;;    (**) Sorting a list of lists according to length of sublists S-99-28
;; a) We suppose that a list contains elements that are lists themselves. The
;; objective is to sort the elements of this list according to their length. E.g.
;; short lists first, longer lists later, or vice versa.
;; 
;; Example:
;; 
;;   (lsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o))) 
;;   ; => ((O) (D E) (D E) (M N) (A B C) (F G H) (I J K L)) 
;; 
;; b) Again, we suppose that a list contains elements that are lists themselves.
;; But this time the objective is to sort the elements of this list according to
;; their length frequency; i.e., in the default, where sorting is done
;; ascendingly, lists with rare lengths are placed first, others with a more
;; frequent length come later.
;; 
;; Example:
;; 
;;   (lfsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o))) 
;;   ; => ((i j k l) (o) (a b c) (f g h) (d e) (d e) (m n)) 
;; 
;; Note that in the above example, the first two lists in the result have length 4
;; and 1, both lengths appear just once. The third and forth list have length 3
;; which appears twice (there are two list of this length). And finally, the last
;; three lists have length 2. This is the most frequent length. 


;;    (**) Determine whether a given integer number is prime. S-99-31
;; Examples:
;;   (prime? 3)
;;   ; => #t
;;   (prime? 4)
;;   ; => #f


;;    (**) Determine the greatest common divisor of two positive integer numbers. S-99-32
;; Examples:
;;
;;   (gcd 3 5)
;;   ; => 1
;;   (gcd 4 6)
;;   ; => 2
;;   (gcd 4 9)
;;   ; => 1


;;    (*) Determine whether two positive integer numbers are coprime. S-99-33
;; Two integers are coprime if the only divisor of both is 1.
;; You can use the greatest common divisor solution from previous problem.
;;
;; Examples:
;;
;;   (coprime? 3 5)
;;   ; => #t
;;   (coprime? 4 6)
;;   ; => #f
;;   (coprime? 4 9)
;;   ; => #t


;;    (**) Calculate Euler's totient function phi(m). S-99-34
;;    (**) Determine the prime factors of a given positive integer. S-99-35
;;    (**) Determine the prime factors of a given positive integer (2). S-99-36
;;    (**) Calculate Euler's totient function phi(m) (improved). S-99-37
;;    (*) Compare the two methods of calculating Euler's totient function. S-99-38?
;;    (*) A list of prime numbers. S-99-39
;;    (**) Goldbach's conjecture. S-99-40?
;;    (**) A list of Goldbach compositions. S-99-41
;;    (**) Truth tables for logical expressions. S-99-46?
;;    (*) Truth tables for logical expressions (2). S-99-47
;;    (**) Truth tables for logical expressions (3). S-99-48?
;;    (**) Gray code. S-99-49?
;;    (***) Huffman code. S-99-50?
;;    (*) Check whether a given term represents a binary tree S-99-54A
;;    (**) Construct completely balanced binary trees S-99-55?
;;    (**) Symmetric binary trees S-99-56?
;;    (**) Binary search trees (dictionaries) S-99-57?
;;    (**) Generate-and-test paradigm S-99-58?
;;    (**) Construct height-balanced binary trees S-99-59?
;;    (**) Construct height-balanced binary trees with a given number of nodes S-99-60?
;;    (*) Count the leaves of a binary tree S-99-61
;;    (*) Collect the leaves of a binary tree in a list S-99-61A
;;    (*) Collect the internal nodes of a binary tree in a list S-99-62?
;;    (*) Collect the nodes at a given level in a list S-99-62B
;;    (**) Construct a complete binary tree S-99-63
;;    (**) Layout a binary tree (1) S-99-64
;;    (**) Layout a binary tree (2) S-99-65
;;    (***) Layout a binary tree (3) S-99-66
;;    (**) A string representation of binary trees S-99-67?
;;    (**) Preorder and inorder sequences of binary trees S-99-68?
;;    (**) Dotstring representation of binary trees S-99-69
;;    (**) Tree construction from a node string S-99-70?
;;    (*) Check whether a given term represents a multiway tree S-99-70B?
;;    (*) Count the nodes of a multiway tree S-99-70C?
;;    (*) Determine the internal path length of a tree S-99-71?
;;    (*) Construct the bottom-up order sequence of the tree nodes S-99-72?
;;    (**) Lisp-like tree representation S-99-73?
;;    (***) Conversions S-99-80
;;    (**) Path from one node to another one S-99-81?
;;    (*) Cycle from a given node S-99-82?
;;    (**) Construct all spanning trees S-99-83?
;;    (**) Construct the minimal spanning tree S-99-84
;;    (**) Graph isomorphism S-99-85?
;;    (**) Node degree and graph coloration S-99-86?
;;    (**) Depth-first order graph traversal (alternative solution) S-99-87?
;;    (**) Connected components (alternative solution) S-99-88?
;;    (**) Bipartite graphs S-99-89?
;;    (**) Eight queens problem S-99-90?
;;    (**) Knight's tour S-99-91?
;;    (***) Von Koch's conjecture S-99-92?
;;    (***) An arithmetic puzzle S-99-93
;;    (***) Generate K-regular simple graphs with N nodes S-99-94
;;    (**) English number words S-99-95?
;;    (**) Syntax checker (alternative solution with difference lists) S-99-96?
;;    (**) Sudoku S-99-97?
;;    (***) Nonograms S-99-98?
;;    (***) Crossword puzzle S-99-99? 
