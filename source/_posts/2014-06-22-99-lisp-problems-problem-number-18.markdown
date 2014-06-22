---
layout: post
title: "99 LISP problems: Problem #18"
date: 2014-06-22 11:59:32 +0530
comments: true
categories: 
- LISP
- 99-problems
---

Solution to the [99 LISP Problems][99prob] #18

Very similar to the solution for #16 eariler.

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
```

Lisp dialect: [Steel Bank Common Lisp][sbcl]

<!--links-->
[99prob]: http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html
[sbcl]: http://www.sbcl.org/
