---
layout: post
title: "99 LISP problems: Problem #19"
date: 2014-06-22 12:38:27 +0530
comments: true
categories: 
- lisp
- 99-problems
---

Solution to the [99 LISP Problems][99prob] #19

Uses the two previous solutions.


```cl

(defun slice (alist from to)
  (let ((i 1))
    (mapcan
      (lambda (x)
        (let ((j i))
          (setf i (1+ i))
          (if (and (>= j from) (<= j to))
            (list x) nil)))
      alist)))

(defun split (alist n)
  (list
    (slice alist 1 n)
    (slice alist (1+ n) (length alist))))

(defun rotate (alist n)
  (let* ((m (if (>= n 0) n (+ (length alist) n)))
         (pieces (split alist m)))
    (append (cadr pieces) (car pieces))))

```


Lisp dialect: [Steel Bank Common Lisp][sbcl]

<!--links-->
[99prob]: http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html
[sbcl]: http://www.sbcl.org/
