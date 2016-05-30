---
layout: post
title: "R: Finding identifier variables"
date: 2014-04-03 15:31:33 +0530
comments: true
categories: 
- R
- id
- STATA
---

I had never expected such a problem, much less a [solution][isid], to exist
till I was asked yesterday to solve it. The problem statement: given a dataset
and a list of candidate variables, find which minimal combination, if any, is a
valid identifier for the observations in the dataset.

<!--more-->

Following is a generic with methods for `matrix`, `data.frame`, and `data.table`.


```r Implementations of isId & findId

isId = function (x, candidate, ...) UseMethod("isId", x)

isId.default = function (x, candidate, ...)
  return(!anyDuplicated(x[ , candidate], ...))

isId.data.table = function (x, candidate)
  return(!any(duplicated(x, by=candidate, with=FALSE)))

findId = function (x, candidateVars, minCombn=2L,
                   maxCombn=length(candidateVars), ...) {
  candidates = unlist(lapply(X=seq.int(minCombn, maxCombn),
                             FUN=combn, x=candidateVars,
                             simplify=FALSE),
                      recursive=FALSE)
  for(cand in candidates) if (isId(x, cand, ...)) return(cand)
  print("Failed. Try other variables.")
  return(invisible(NULL))
}

```


Here are some tests for the implemenation above.


```r Testing the functions

############
#  Test 1  #
############

set.seed(pi)
testDF = data.frame(id=letters[1:10],
                    a=letters[sample.int(5, 10, TRUE)],
                    b=LETTERS[sample.int(3, 10, TRUE)],
                    e=runif(10),
                    f=rnorm(10))

sapply(names(testDF), isId, x=testDF)
#   id     a     b     e     f 
# TRUE FALSE FALSE  TRUE  TRUE

findId(testDF, c("id"), 1)
#[1] "id"

findId(testDF, c("a", "b", "id"), 2)
#[1] "a" "id"

############
#  Test 2  #
############

data(iris)
findId(iris, names(iris), 1)
#[1] "Failed. Try other variables."

iris[["id"]] = 1:nrow(iris)
findId(iris, names(iris), 1)
#[1] "id"

```


The `minCombn` and `maxCombn` variables are used to specify how many candidate
columns must be considered together to find an identifier. `...` can be used to
pass further arguments to `anyDuplicated`. The code is smart enough to try all
`k` variable combinations before attempting any combination of `k + 1`
variables.

# Benchmarking

The `data.table` method for this function was implemented after [this discussion][idso]
at StackOverflow. Therefore, I use a similar example to show the efficiency of
the two methods here:


```r Benchmarking data.frame vs. data.table

library(data.table)
library(microbenchmark)

set.seed(pi)
df = data.frame(a=sample(100, 1e7, TRUE), 
                b=sample(letters, 1e7, TRUE), 
                id=seq.int(1e7))
dt <- as.data.table(df)

microbenchmark(system.time(isId(df, "id")),
               system.time(isId(dt, "id")),
               system.time(isId(df, c("a", "id"))),
               system.time(isId(dt, c("a", "id"))),
               system.time(isId(df, c("a", "b", "id"))),
               system.time(isId(dt, c("a", "b", "id"))),
               times=3L)
#Unit: milliseconds
#                                     expr        min         lq     median          uq        max neval
#              system.time(isId(df, "id"))   599.8991   644.8919   689.8848    808.0744   926.2641     3
#              system.time(isId(dt, "id"))  1877.0710  1893.4972  1909.9234   2843.8230  3777.7225     3
#      system.time(isId(df, c("a", "id"))) 34398.9664 35028.6384 35658.3105  52713.9050 69769.4995     3
#      system.time(isId(dt, c("a", "id")))  4013.6891  4017.2014  4020.7137   4027.9836  4035.2535     3
# system.time(isId(df, c("a", "b", "id"))) 39915.0490 48370.9638 56826.8785  70350.0956 83873.3127     3
# system.time(isId(dt, c("a", "b", "id")))  6993.5077  7642.8285  8292.1493   8342.6799  8393.2105     3

```


We can see that the `default` method is faster when evaluating a single column
as id. However, that is the only case where it does better. It scales poorly;
especially if compared against the `data.table` method. When you are searching
for an identifier you probably are going to try various combinations and
`data.table` will almost always be much faster.

<!--links-->
[isid]: http://www.stata.com/help.cgi?isid
[idso]: http://stackoverflow.com/questions/22834725/r-checking-if-a-set-of-variables-forms-a-unique-index
