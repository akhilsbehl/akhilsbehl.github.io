---
layout: post
title: "99 LISP problems: Problem #3"
date: 2014-06-20 00:22:17 +0530
comments: true
categories: 
- LISP
- 99-problems
---

Solution to the [99 LISP Problems][99prob] #3:


```cl

(defun element-at (alist n)
  (if (< (length alist) n)
    nil
    (if (= n 1)
      (car alist)
      (element-at (cdr alist) (1- n)))))

```

Lisp dialect: [Steel Bank Common Lisp][sbcl]

<!--links-->
[99prob]: http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html
[sbcl]: http://www.sbcl.org/
