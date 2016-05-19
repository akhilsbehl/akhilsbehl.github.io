---
layout: post
title: "99 LISP problems: Problem #34"
date: 2014-08-06 03:23:34 +0530
comments: true
categories: 
- LISP
- 99-problems
---

Solution to the [99 LISP Problems][99prob] #34


```cl

(defun range (lo hi)
    (cond ((> lo hi) nil)
    (t (cons lo (range (1+ lo) hi)))))

(defun eucgcd (m n)
    (let ((p (abs m)) (q (abs n)))
        (cond ((= m n) m)
        (t (gcd (- (max m n) (min m n)) (min m n))))))

(defun coprimep (m n)
    (= (eucgcd m n) 1))

(defun totient (m)
  (cond ((= m 1) 1)
        (t (apply #'+ (mapcar
                        (lambda (x) (if (coprimep m x) 1 0))
                        (range 1 (1- m)))))))

```


Lisp dialect: [Steel Bank Common Lisp][sbcl]

<!--links-->
[99prob]: http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html
[sbcl]: http://www.sbcl.org/
