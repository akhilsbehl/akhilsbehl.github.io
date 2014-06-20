---
layout: post
title: "99 LISP problems: Problem #14"
date: 2014-06-20 15:38:59 +0530
comments: true
categories: 
- LISP
- 99-problems
---

Solution to the [99 LISP Problems][99prob] #14

```cl
(defun dupli (alist)
  (if (null alist)
    nil
    (append
      (list (car alist) (car alist))
      (dupli (cdr alist)))))
```

Lisp dialect: [Steel Bank Common Lisp][sbcl]

<!--links-->
[99prob]: http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html
[sbcl]: http://www.sbcl.org/
