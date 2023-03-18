(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch
                (car bits)
                current-branch)))
          (if (leaf? next-branch)
              (cons
               (symbol-leaf next-branch)
               (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits)
                        next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit:
               CHOOSE-BRANCH" bit))))
(define (encode message tree)
  (if (null? message)
      '()
      (append
       (encode-symbol (car message) tree)
       (encode (cdr message) tree))))

(define (encode-symbol sym tree)
  (cond ((leaf? tree) '())
        ((memq sym (symbols (left-branch tree)))
         (cons 0 (encode-symbol sym (left-branch tree))))
        ((memq sym (symbols (right-branch tree)))
         (cons 1 (encode-symbol sym (right-branch tree))))
        (else (error "symbol not in tree:
               ENCODE-SYMBOL" sym))))

(define (make-pair sym freq)
  (list sym freq))
(define (symbol-pair p) (car p))
(define (freq-pair p) (cadr p))

(define (generate-huffman-tree pairs)
  (successive-merge
   (make-leaf-set pairs)))

(define (successive-merge leaves)
  (if (null? (cdr leaves))
      (car leaves)
      (successive-merge
       (adjoin-set (make-code-tree (car leaves) (cadr leaves))
                   (cddr leaves)))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set
         (make-leaf (symbol-pair pair)
                    (freq-pair pair))
         (make-leaf-set (cdr pairs))))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define sample-tree
  (generate-huffman-tree
   '((a 5) (b 2) (r 2) (c 1) (d 1))))

(define sample-message
  '(a b r a c a d a b r a))

(decode (encode sample-message sample-tree) sample-tree)
