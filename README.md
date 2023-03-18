# sicp-huffman

``` scheme
(define sample-tree
  (generate-huffman-tree
   '((a 5) (b 2) (r 2) (c 1) (d 1))))

(define sample-message
  '(a b r a c a d a b r a))

(decode (encode sample-message sample-tree) sample-tree)

;; > (a b r a c a d a b r a)
```

