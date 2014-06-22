---
layout: post
title: "99 LISP problems: Problem #20"
date: 2014-06-22 12:47:36 +0530
comments: true
categories: 
- LISP
- 99-problems
---

Solution to the [99 LISP Problems][99prob] #20

```cl
(defun remove-at (alist n)
  (let ((i 1))
    (mapcan
      (lambda (x)
        (let ((j i))
          (setf i (1+ i))
          (if (= j n) nil (list x))))
      alist)))
```

Lisp dialect: [Steel Bank Common Lisp][sbcl]

<!--links-->
[99prob]: http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html
[sbcl]: http://www.sbcl.org/
