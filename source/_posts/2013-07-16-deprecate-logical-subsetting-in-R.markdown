---
layout: post
title: "Deprecate logical subsetting in R"
date: 2013-07-16 21:09
comments: true
categories: R
---

Since I discovered it, I have found logical subsetting in R a very elegant idiom. Recently I've had a change of heart due to two observations. Here is why I propose staying away from it.

<!--more-->
First of all, there is this [question][soq] on stackoverflow which proves that subsetting numerically using `which` is faster than logical subsetting.

I know this may not be a good enough reason for some of you. It wasn't for me either till I found this **bug**.[^1] It's a subtle, and therefore, the sinister kind. See the code snippet below to reproduce the bug.

```r
## Suppose you get an empty data.frame from somewhere. Probably from
## logical subsetting itself. For example:
R> xx <- data.frame("u"=runif(10), "n"=rnorm(10))

## Now let's get an empty data.frame:
R> xx[FALSE]
data frame with 0 columns and 10 rows

# But wait! That's got 10 rows? Anyway, let's continue.
R> xx <- xx[FALSE]

## Now, you are happily coding away when you find your code
## splutters and throws some garbage error message.
## What happened? May be this:

R> yy3 <- xx[c(TRUE, FALSE, TRUE, FALSE, TRUE), ]

R> yy3
'data.frame':   3 obs. of  0 variables

R> nrow(yy3) # Btw, NROW, dim all have this bug.
[1] 3

R> attributes(yy3)
$names
character(0)

$row.names
[1] "NA"   "NA.1" "NA.2"

$class
[1] "data.frame"

## You, the oblivious one, have checked for nrow and go to do
## something innocent such as this:

R> sum(yy[1, ])
Error in FUN(X[[1L]], ...) : 
  only defined on a data frame with all numeric variables
Calls: Summary.data.frame -> lapply -> FUN

## What the #@$*@ !!!
```

And you are left wondering where you called `summary` in your code.

Even if one discounts efficiency, it may be helpful to give up on the idiom of logical subsetting for reasons of correctness. At least for `data.frame`s.

***
<!-- Footnotes -->
[^1]: Or at least I'll consider this a bug.

<!-- Links -->
[soq]: http://stackoverflow.com/questions/17510778/why-is-subsetting-on-a-logical-type-slower-than-subetting-on-numeric-type
