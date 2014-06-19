---
layout: post
title: "99 LISP problems: Problem #2"
date: 2014-06-20 00:10:33 +0530
comments: true
categories: 
- LISP
- 99-problems
---

Solution to the [99 LISP Problems][99prob] #2:

```cl
(defun my-but-last (alist)
  (if
    (<= (length alist) 2)
    alist
    (my-but-last (cdr alist))))
```

Lisp dialect: [Steel Bank Common Lisp][sbcl]

<!--links-->
[99prob]: http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html
[sbcl]: http://www.sbcl.org/
