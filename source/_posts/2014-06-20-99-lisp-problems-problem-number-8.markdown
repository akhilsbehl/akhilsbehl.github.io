---
layout: post
title: "99 LISP problems: Problem #8"
date: 2014-06-20 01:28:38 +0530
comments: true
categories: 
- LISP
- 99-problems
---

Solution to the [99 LISP Problems][99prob] #8


```cl

(defun compress (alist)
  (if (null alist)
    nil
    (append
      (if (equalp
            (car alist)
            (car (cdr alist)))
        nil
        (list (car alist)))
      (compress (cdr alist)))))

```


Lisp dialect: [Steel Bank Common Lisp][sbcl]

<!--links-->
[99prob]: http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html
[sbcl]: http://www.sbcl.org/
