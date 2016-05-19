---
layout: post
title: "99 LISP problems: Problem #12"
date: 2014-06-20 14:48:28 +0530
comments: true
categories: 
- lisp
- 99-problems
---

Solution to the [99 LISP Problems][99prob] #12


```cl

(defun decompress-one (n elem)
  (if (<= n 0)
    nil
    (append
      (list elem)
      (decompress-one (1- n) elem))))

(defun decode-rle (encoding)
  (if (null encoding)
    nil
    (append
      (if (listp (car encoding))
        (decompress-one (caar encoding) (second (car encoding)))
        (list (car encoding)))
      (decode-rle (cdr encoding)))))

```


Lisp dialect: [Steel Bank Common Lisp][sbcl]

<!--links-->
[99prob]: http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html
[sbcl]: http://www.sbcl.org/
