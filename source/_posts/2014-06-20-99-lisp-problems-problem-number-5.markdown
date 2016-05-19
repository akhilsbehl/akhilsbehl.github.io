---
layout: post
title: "99 LISP problems: Problem #5"
date: 2014-06-20 00:50:19 +0530
comments: true
categories: 
- LISP
- 99-problems
---

Solution to the [99 LISP Problems][99prob] #5


```cl

(defun my-reverse (alist)
  (if (null alist)
    nil
    (append
      (my-reverse (cdr alist))
      (list (car alist)))))

```


Lisp dialect: [Steel Bank Common Lisp][sbcl]

<!--links-->
[99prob]: http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html
[sbcl]: http://www.sbcl.org/
