---
layout: post
title: "99 LISP problems: Problem #11"
date: 2014-06-20 14:09:00 +0530
comments: true
categories: 
- LISP
- 99-problems
---

Solution to the [99 LISP Problems][99prob] #11

```cl
(defun encode-modified (alist)
  (if (null alist)
    nil
    (append
      (if (= (length (pack-first alist)) 1)
        (list (car alist))
        (list (list (length (pack-first alist)) (car alist))))
      (encode-modified (trim-first alist)))))
```

Lisp dialect: [Steel Bank Common Lisp][sbcl]

<!--links-->
[99prob]: http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html
[sbcl]: http://www.sbcl.org/
