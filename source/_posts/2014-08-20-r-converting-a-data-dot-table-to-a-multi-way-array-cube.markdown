---
layout: post
title: "R: Converting a data.table to a multi way array (cube)"
date: 2014-08-20 15:42:00 +0530
comments: true
categories: 
- R
- cube
- data.table
---

This post discusses the problem of converting a `data.table` to the `array` data structure in R. The idea is analogous to converting a denormalized dataset that presents both dimensions and facts in a table as columns of the table to a completely normalized fact cube along the dimensions of the dataset.

The problem can be solved in multiple ways in R with attending constraints of these approaches -- e.g. `plyr::daply`, `xtabs`, `by` or a manual home-brewn set of split and lapply routine. Without discussing the constraints I observed with the existing techniques, I am presenting an alternative approach here that depends on unrolling the rectangular data structure into a linear structure and then reshaping it by manually counting the facts and dimension sizes (think strides). The choice of using a `data.table` was purely for efficiency reasons but the same idea can be implemented with a `data.frame` with little changes to the code.

<!--more-->

Dislcaimer: A constraint (I see it as essentially the functionality being implemented here) that all rows should be unique along the chosen dimensions. Since a cube must have all dimensions perfectly normalized.

Following is the implemenation with an example:


```r

dt2array = function (x, facts, dims) {
  stopifnot(is.data.table(x))
  setkeyv(x, rev(dims))
  stopifnot(!any(duplicated(x)))
  dimensions = lapply(x[ , rev(dims), with=FALSE],
                      function (x) sort(unique(x)))
  xFull = data.table(expand.grid(dimensions, stringsAsFactors=FALSE))
  setkeyv(xFull, rev(dims))
  x = data.table:::merge.data.table(xFull, x, by=dims, all=TRUE)
  factsVec = unlist(x[ , facts, with=FALSE], recursive=FALSE, use.names=FALSE)
  nFacts = length(facts)
  nDims = length(dims)
  if (nFacts > 1) {
    dim(factsVec) = c(sapply(dimensions, length), nFacts)
    dimnames(factsVec) = c(dimensions, "facts"=list(facts))
    return(aperm(factsVec, perm=c(nDims:1, nDims + 1)))
  } else {
    dim(factsVec) = sapply(dimensions, length)
    dimnames(factsVec) = dimensions
    return(aperm(factsVec))
  }
}

dat = data.table(f1=runif(10),
                 f2=runif(10),
                 f3=runif(10),
                 d1=letters[1:5],
                 d2=rep(letters[3:4], each=5),
                 d3=LETTERS[1:2])
dt2array(dat, "f1", c("d1", "d2", "d3"))
dt2array(dat, c("f1", "f2"), c("d1", "d2", "d3"))
dt2array(dat, c("f1", "f2", "f3"), c("d1", "d2", "d3"))

```


The implementation is very fast, if I say so myself, since almost all manipulations being used here are fairly low-level in R implemented in C. Hopefully, this will be useful.
