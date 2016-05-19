---
layout: post
title: "99 LISP problems: Problem #22"
date: 2014-06-22 12:57:08 +0530
comments: true
categories: 
- LISP
- 99-problems
---

Solution to the [99 LISP Problems][99prob] #22


```cl

(defun range-inner (lo hi)
  (if (> lo hi)
    nil
    (append
      (list lo)
      (range-inner (1+ lo) hi))))

(defun range (left right)
  (if (< right left)
    (reverse (range-inner right left))
    (range-inner left right)))

```


Lisp dialect: [Steel Bank Common Lisp][sbcl]

<!--links-->
[99prob]: http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html
[sbcl]: http://www.sbcl.org/
