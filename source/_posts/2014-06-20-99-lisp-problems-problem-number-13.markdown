---
layout: post
title: "99 LISP problems: Problem #13"
date: 2014-06-20 15:24:56 +0530
comments: true
categories: 
- LISP
- 99-problems
---

Solution to the [99 LISP Problems][99prob] #13

I didn't enjoy this problem very much. The solution is very similar to that for
#9. But sticks to the requirements of the problem (count directly without
making sublists).

```cl
(defun count-first (alist)
  (if (null alist) 0
    (if (equalp (car alist) (cadr alist))
      (1+ (count-first (cdr alist))) 1)))

(defun trim-first (alist)
  (if (null alist)
    nil
    (if (not (equalp
               (car alist)
               (cadr alist)))
      (cdr alist)
      (trim-first (cdr alist)))))

(defun encode-direct (alist)
  (if (null alist)
    nil
    (append
      (if (= (count-first alist) 1)
        (list (car alist))
        (list (list (count-first alist) (car alist))))
      (encode-direct (trim-first alist)))))
```

Lisp dialect: [Steel Bank Common Lisp][sbcl]

<!--links-->
[99prob]: http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html
[sbcl]: http://www.sbcl.org/
