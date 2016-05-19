---
layout: post
title: "99 LISP problems: Problem #23"
date: 2014-06-28 15:54:31 +0530
comments: true
categories: 
- LISP
- 99-problems
---

Solution to the [99 LISP Problems][99prob] #23


```cl

(defun element-at (alist n)
  (if (< (length alist) n)
    nil
    (if (= n 1)
      (car alist)
      (element-at (cdr alist) (1- n)))))

(defun remove-at (alist n)
  (let ((i 1))
    (mapcan
      (lambda (x)
        (let ((j i))
          (setf i (1+ i))
          (if (= j n) nil (list x))))
      alist)))

(defun rnd-select (alist n)
  (if (<= n 0)
    nil
    (let ((k (1+ (random (length alist)))))
      (append
        (list (element-at alist k))
        (rnd-select (remove-at alist k) (1- n))))))

```


Lisp dialect: [Steel Bank Common Lisp][sbcl]

<!--links-->
[99prob]: http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html
[sbcl]: http://www.sbcl.org/
