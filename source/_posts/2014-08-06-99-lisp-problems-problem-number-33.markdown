---
layout: post
title: "99 LISP problems: Problem #33"
date: 2014-08-06 02:08:27 +0530
comments: true
categories: 
- LISP
- 99-problems
---

Solution to the [99 LISP Problems][99prob] #33

```cl
(defun eucgcd (m n)
  (let ((p (abs m)) (q (abs n)))
    (cond ((= m n) m)
          (t (gcd (- (max m n) (min m n)) (min m n))))))

(defun coprimep (m n)
  (= (eucgcd m n) 1))
```

Lisp dialect: [Steel Bank Common Lisp][sbcl]

<!--links-->
[99prob]: http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html
[sbcl]: http://www.sbcl.org/
