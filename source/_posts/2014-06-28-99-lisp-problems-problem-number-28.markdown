---
layout: post
title: "99 LISP problems: Problem #28"
date: 2014-06-28 15:54:32 +0530
comments: true
categories: 
- LISP
- 99-problems
---

Solution to the [99 LISP Problems][99prob] #28

```cl
;;; Part a)
(defun lsort (alist)
  (sort
    alist
    #'<
    :key #'length))

;;; Part b)
(defun count-elem (alist elem)
  (apply
    #'+
    (mapcar
      (lambda (x)
        (if (equalp x elem) 1 0))
      alist)))

(defun lfsort (alist)
  (let ((lengths (mapcar #'length alist)))
    (sort
      alist #'<
      :key (lambda (x) (count-elem lengths (length x))))))
```

Lisp dialect: [Steel Bank Common Lisp][sbcl]

<!--links-->
[99prob]: http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html
[sbcl]: http://www.sbcl.org/
