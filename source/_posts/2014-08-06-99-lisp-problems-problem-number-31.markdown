---
layout: post
title: "99 LISP problems: Problem #31"
date: 2014-08-06 02:11:59 +0530
comments: true
categories: 
- LISP
- 99-problems
---

Solution to the [99 LISP Problems][99prob] #31


```cl

(defun range (lo hi)
  (cond ((> lo hi) nil)
        (t (cons lo (range (1+ lo) hi)))))

(defun all (alist)
  (cond ((null alist) t)
        (t (and (car alist)
                (all (cdr alist))))))

(defun primep (n)
  (all (mapcar
         (lambda (x) (not (= 0 (mod n x))))
         (range 2 (isqrt n)))))

```


Lisp dialect: [Steel Bank Common Lisp][sbcl]

<!--links-->
[99prob]: http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html
[sbcl]: http://www.sbcl.org/
