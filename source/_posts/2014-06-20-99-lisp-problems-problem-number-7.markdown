---
layout: post
title: "99 LISP problems: Problem #7"
date: 2014-06-20 01:18:21 +0530
comments: true
categories: 
- LISP
- 99-problems
---

Solution to the [99 LISP Problems][99prob] #7


```cl

(defun my-flatten (alist)
  (if (null alist)
    nil
    (append
      (if (listp (car alist))
        (my-flatten (car alist))
        (list (car alist)))
      (my-flatten (cdr alist)))))

```


Lisp dialect: [Steel Bank Common Lisp][sbcl]

<!--links-->
[99prob]: http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html
[sbcl]: http://www.sbcl.org/
