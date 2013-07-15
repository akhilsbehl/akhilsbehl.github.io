---
layout: post
title: "Recrating the history of a stock index's membership in R"
date: 2013-07-15 01:40
comments: true
publish: false
categories:
- R
- Finance
- Time Series
---

If you work in finance, have you ever needed to identify the stocks that were in an index at any given date? Or perhaps a series of dates? Given how central indices are to financial markets and how frequently they change, this is a common problem.

<!--more-->

The rub is that this common problem seems too trivial till you sit down to solve it. Hopefully, this post will save you a couple of hours the next time you come across this problem (and if you are a user of R). In this post I am going to present a general method to recreate a time series of membership of a set using the current membership and a list of changes in it.

There are a few subtle points to it that we'll discuss shortly. But first let's start with a very simple example.

```r
# Let's have three time periods.
periods <- 3

# And name the change times as `t-1', `t-2', and `t-3'.
ctime <- paste0("t-", seq_len(periods))

# Define current membership of a set of alphabets.
current <- letters[1:5]

# Let's assume that "a" was added to the set at `t-1' and so on...
added <- setNames(c("a", "b", "c"), ctime)
added

# Similarly the ones that were removed:
removed <- setNames(c("f", "g", "h"), ctime)
removed

# Let's use the power of `wishful thinking' and hope we have something which
# could give us this:
create_mem_ts(ctime, added, removed, current)

$current
[1] "a" "b" "c" "d" "e"

$`t-1`
[1] "b" "c" "d" "e" "f"

$`t-2`
[1] "c" "d" "e" "f" "g"

$`t-3`
[1] "d" "e" "f" "g" "h"

attr(,"index")
[1] "t-1" "t-2" "t-3"
```
