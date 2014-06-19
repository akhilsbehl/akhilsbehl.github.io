---
layout: post
title: "99 LISP problems: Problem #4"
date: 2014-06-20 00:49:55 +0530
comments: true
categories: 
- LISP
- 99-problems
---

Solution to the [99 LISP Problems][99prob] #4

```cl
(defun my-len (alist)
  (if (null alist)
    0
    (1+ (my-len (cdr alist)))))
```

Lisp dialect: [Steel Bank Common Lisp][sbcl]

<!--links-->
[99prob]: http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html
[sbcl]: http://www.sbcl.org/
