---
layout: post
title: "R: Converting simple_triplet_matrix to other sparse matrix formats."
date: 2014-11-21 14:17:11 +0530
comments: true
categories: 
- R
- sparse
- matrix
---

When working with the `tm` package in R, it produces a `DocumentTermMatrix` or `TermDocumentMatrix` as an S3 object of class `simple_triplet_matrix` from the package `slam`. Now R has various different packages for creating sparse matrices, each of which have packages depending upon themselves. Long back when working with text data I had created two functions to convert a `simple_triplet_matrix` to a matrix of class `sparseMatrix` (package: `Matrix`) and that to a matrix of class `matrix.csr` (package: `SparseM`). The conversions are simple enough once you have read through the documentation of the three packages but hopefully this will save someone some time.

```r
asSparseMatrix = function (simpleTripletMatrix) {
  retVal = sparseMatrix(i=simpleTripletMatrix[["i"]],
                        j=simpleTripletMatrix[["j"]],
                        x=simpleTripletMatrix[["v"]],
                        dims=c(simpleTripletMatrix[["nrow"]],
                               simpleTripletMatrix[["ncol"]]))
  if (!is.null(simpleTripletMatrix[["dimnames"]]))
    dimnames(retVal) = simpleTripletMatrix[["dimnames"]]
  return(retVal)
}

asMatrixCSR = function (sparseMatrix) {
  warning("Will lose dimnames!")
  as.matrix.csr(new("matrix.csc",
                    ra=sparseMatrix@x,
                    ja=sparseMatrix@i + 1L,
                    ia=sparseMatrix@p + 1L,
                    dimension=sparseMatrix@Dim))
}
```
