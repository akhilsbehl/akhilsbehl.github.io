---
layout: post
title: "R: textConnection for string IO"
date: 2014-02-09 12:26:14 +0530
comments: true
publish: false
categories:
- R
- textConnection
- string IO
---

I've used `textConnection` in R for reading from strings. Only recently, I realized (duh!) that it may also be used for write functionality much like Python's stringIO module. In this post I show some simple examples for how to use this functionality.

<!--more-->

## Reading from a text connection.

Probably the most common places we use a text connection for input is when using the `scan` command. In fact, `scan` provides a simple optional argument to handle this special case. The fact that `read.table` (and derivatives) rely on `scan` under the hood makes this usable there as well.


```r

R> myString = "1 2 3 4 5 6 7 8 9 10"
R> scan(text=myString, what=1L)
Read 10 items
 [1]  1  2  3  4  5  6  7  8  9 10

R> myString = "1 2 3 4 5\n6 7 8 9 10"
R> read.table(text=myString, sep=" ")
  V1 V2 V3 V4 V5
1  1  2  3  4  5
2  6  7  8  9 10

```


I can't think of any other cases where I have used this functionality personally. If you have any other common (or even exotic) use-cases where this was useful, let me know in the comments.

## Writing out to a textConnection:

Consider the case where one may want to collect all output from the R console as text to parse for some further analysis. `sink` is a useful command in this case. One may need to use `sink` while bypassing the filesystem (perhaps on a diskless system?!). `textConnection` to the rescue.

Here is an example:


```r

R> textSink = ""
R> sinkCon = textConnection("textSink", open="w")
R> sinkCon
     description            class             mode             text           opened         can read        can write 
      "textSink" "textConnection"              "w"           "text"         "opened"             "no"            "yes"

```


Look at the `mode` and `can write` parts of the description above. Now let's try actually writing to it.


```r

R> sink(sinkCon)
R> summary(lm(runif(1e3) ~ rnorm(1e3))) # There will be no output on the console.
R> close(sinkCon) # Make sure to close the sink.

# Let's look at what is in the textSink
R> textSink
 [1] ""                                                               "Call:"                                                         
 [3] "lm(formula = runif(1000) ~ rnorm(1000))"                        ""                                                              
 [5] "Residuals:"                                                     "     Min       1Q   Median       3Q      Max "                 
 [7] "-0.52214 -0.25361  0.01119  0.25541  0.52547 "                  ""                                                              
 [9] "Coefficients:"                                                  "             Estimate Std. Error t value Pr(>|t|)    "         
[11] "(Intercept)  0.501199   0.009238  54.256   <2e-16 ***"          "rnorm(1000) -0.023373   0.009168  -2.549   0.0109 *  "         
[13] "---"                                                            "Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1"
[15] ""                                                               "Residual standard error: 0.292 on 998 degrees of freedom"      
[17] "Multiple R-squared:  0.006471,\tAdjusted R-squared:  0.005475 " "F-statistic:   6.5 on 1 and 998 DF,  p-value: 0.01094"         
[19] ""                                                              

```


## Epilogue

Clearly, we have full capabilities to do IO to and from strings in R. However, the use-cases where string IO is the best idea still eludes me. I will love to hear from other users who have or may use this for some idea of their own.
