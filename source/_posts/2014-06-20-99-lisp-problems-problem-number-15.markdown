---
layout: post
title: "99 LISP problems: Problem #15"
date: 2014-06-20 15:48:56 +0530
comments: true
categories: 
- LISP
- 99-problems
---

Solution to the [99 LISP Problems][99prob] #15

```cl
(defun repli-one (elem n)
  (if (<= n 0)
    nil
    (append
      (list elem)
      (repli-one elem (1- n)))))

(defun repli (alist n)
  (append
    (mapcan
      (lambda (elem)
        (repli-one elem n))
      alist)))
```

Lisp dialect: [Steel Bank Common Lisp][sbcl]

<!--links-->
[99prob]: http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html
[sbcl]: http://www.sbcl.org/
