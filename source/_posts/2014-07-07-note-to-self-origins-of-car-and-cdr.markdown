---
layout: post
title: "Note to self: Origins of 'car' and 'cdr'"
date: 2014-07-07 13:27:32 +0530
comments: true
categories: 
- common-lisp
- LISP
- functional-programming
---

So, I am rereading the [book][touretzky] _Gentle Introduction to Symbolic Computing_ by Touretzky. I highly recommend the book to people who are new to both lisp and programming. Today's post is simply a quote from the book that I want to preserve as a note to self on the origins of the two fundamental lisp functions: `car` and `cdr`.

{% blockquote %}
These names are relics from the early days of computing, when Lisp first ran on a machine called the IBM 704. The 704 was so primitive it didn't even have transistors -- it used vacuum tubes. Each of its 'registers' was divided into several components, two of which were the address portion and the decrement portion. Back then, the name CAR stood for Contents of Address portion of Register, and CDR stood for Contents of Decrement portion of Register.
{% endblockquote %}

<!--links-->
[touretzky]: http://www.cs.cmu.edu/~dst/LispBook/book.pdf
