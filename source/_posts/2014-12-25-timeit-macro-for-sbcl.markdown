---
layout: post
title: "`timeit` macro for SBCL"
date: 2014-12-25 05:23:28 +0530
comments: true
categories:
- lisp
- profiling
- macro
---

I am the sort of person who really likes to know how much time code I have written takes to run. I believe that it is important to know what works in your language and what does not and what sort of efficiency trade offs need to be made for expressiveness and brevity. Since, I am learning common lisp a little seriously again, I have been interested in seeing how to profile code in it. Even though I haven't reached [this][brian] far that I have started using the [statistical CPU profiler][cpuprof] or the [statistical allocation profiler][allocprof], I am starting out with simply being able to time code I write quickly.

Since, writing macros in lisp is so easy and timing code is just the sort of thing macros are really useful for, I decided to practice some macro writing to write a `timeit` macro. Similar [attempts][attempt] have been made in the past but I wanted to roll my own. After some [struggle and a little nudge][rainerhelp] I was able to write something satisfactory:


```cl

(defmacro timeit ((&key
                    (to-stream *standard-output*)
                    (with-runs 1))
                  &body body)
  "Note that this function may barf if you are depending on a single evaluation
  and choose with-runs to be greater than one. But I guess that will be the
  caller's mistake instead."
  (let ((start-time (gensym))
        (stop-time (gensym))
        (temp (gensym))
        (retval (gensym))
        (elapsed-time (gensym)))
    `(let* ((,start-time (get-internal-run-time))
            (,retval (let ((,temp))
                       (dotimes (i ,with-runs ,temp)
                         (setf ,temp ,@body))))
            (,stop-time (get-internal-run-time))
            (,elapsed-time (/ (- ,stop-time ,start-time)
                              internal-time-units-per-second)))
       (format ,to-stream
               (concatenate 'string
                            "~CAverage (total) time spent in expression"
                            " over ~:d iterations: ~f (~f) seconds.~C")
               #\linefeed
               ,with-runs
               ,elapsed-time
               (/ ,elapsed-time ,with-runs)
               #\linefeed)
       ,retval)))

```


Hopefully, this is useful for someone not in the mood to write their own code to do this. This was definitely useful for me to write.

<!--links-->
[brian]: http://t-b-o-g.blogspot.in/2009/12/brians-brain-on-common-lisp-take-3.html
[cpuprof]: http://www.sbcl.org/1.0/manual/Statistical-Profiler.html
[allocprof]: https://www.snellman.net/blog/archive/2006-05-14-statistical-allocation-profiler-for-sbcl.html
[attempt]: http://www-users.cs.umn.edu/~gini/lisp/profile.html
[rainerhelp]: http://stackoverflow.com/questions/27642626/lisp-defmacro-with-optional-and-body
