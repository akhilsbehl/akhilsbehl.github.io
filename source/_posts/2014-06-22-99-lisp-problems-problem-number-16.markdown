---
layout: post
title: "99 LISP problems: Problem #16"
date: 2014-06-22 11:18:03 +0530
comments: true
categories: 
- LISP
- 99-problems
---

Solution to the [99 LISP Problems][99prob] #16

This seemingly innocuous problem turned out to be a little challenging too.
Like I have mentioned earlier, I strongly prefer a pure functional or recursive
solution. Therefore, carrying the state around (in this case, the position of
the current in the list) is slightly tough. I knew I should be using closures.
I have used some sort of `let` + `lambda`. Not sure if this is elegant.


```cl

(defun drop (alist n)
  (let ((i 1))
    (mapcan
      (lambda (x)
        (let ((j i))
          (setf i (1+ i))
          (if (= (mod j n) 0) nil (list x))))
      alist)))

```


Lisp dialect: [Steel Bank Common Lisp][sbcl]

<!--links-->
[99prob]: http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html
[sbcl]: http://www.sbcl.org/
