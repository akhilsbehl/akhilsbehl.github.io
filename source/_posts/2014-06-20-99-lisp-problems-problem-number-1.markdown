---
layout: post
title: "99 LISP problems: Problem #1"
date: 2014-06-20 00:03:21 +0530
comments: true
categories: 
- LISP
- 99-problems
---

Solution to the [99 LISP Problems][99prob] #1:


```cl

(defun my-last (alist)
  (if
    (equalp (length alist) 0)
    nil
    (if
      (equalp (length alist) 1)
      alist
      (my-last (cdr alist)))))

```


Lisp dialect: [Steel Bank Common Lisp][sbcl]

<!--links-->
[99prob]: http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html
[sbcl]: http://www.sbcl.org/
