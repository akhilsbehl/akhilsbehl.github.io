---
layout: post
title: "99 LISP problems: Problem #9"
date: 2014-06-20 13:35:38 +0530
comments: true
categories: 
- LISP
- 99-problems
---

Solution to the [99 LISP Problems][99prob] #9

This was the first _challenging_ problem I encountered in this set. After
spending the first few minutes, I started worrying that this problem may not
fit into a simple recursive solution. I did not want to write a `do` or `for`
loop etc.

What I was looking for was some ingenuously simple recursive solution to this
problem (without the use of a helper or a lambda). After quite a bit of
struggle (and some looking around) I realized there isn't one (that I could
find, at least). Therefore, I implemented this as a recursive solution by
implementing two new verbs (themselves recursive) specifically for this
problem.

<!--more-->

A couple more points of note:

* I learnt that I can use `(cadr alist)` instead of `(car (cdr alist))`.
* I find it wasteful to wrap everything in a list to use append; e.g.
  `(append (list 'a) (list 'b))` in my functions (see the previous few
  solutions). I wonder if I just don't know a more elegant verb for situations
  like these.

```cl
(defun pack-first (alist)
  (if (null alist)
    nil
    (if (equalp
          (car alist)
          (cadr alist))
      (append (list (car alist)) (pack-first (cdr alist)))
      (list (car alist)))))

(defun trim-first (alist)
  (if (null alist)
    nil
    (if (not (equalp
               (car alist)
               (cadr alist)))
      (cdr alist)
      (trim-first (cdr alist)))))

(defun pack (alist)
  (if (null alist)
    nil
    (append
      (list (pack-first alist))
      (pack (trim-first alist)))))
```

Lisp dialect: [Steel Bank Common Lisp][sbcl]

<!--links-->
[99prob]: http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html
[sbcl]: http://www.sbcl.org/
