---
layout: post
title: "EDA: Plotting least squares fit line in R"
date: 2014-06-04 12:22
comments: true
categories: EDA, R, OLS, plot, ISLR
published: true
---

I have recently started reading [ISLR][islr] and am finding the plots in the
book very useful.

A visualization aid one often uses for exploratory data analysis is a scatter
plot of the response variable against a potential predictor. Overlaying the
ordinary least squares fit line on this scatter provides a readily accessible
visual representation of the effect of the predictor on the response (if any).

Following is a simple snippet that I wrote in R to plot such graphs for any
arbitrary dataset with some numeric response variable. Note that the function
only attempts the plots for predictors which are numeric (or integer). It also
attempts a crude adjustment of the layout of the plot according to the number
of predictors.

<!--more-->


```r Plotting OLS fit of features against the response
plotLeastSqFit = function(df, responseVar) {
    stopifnot(is.data.frame(df), responseVar %in% colnames(df), is.numeric(df[[responseVar]]))
    areNumeric = setdiff(colnames(df)[sapply(df, is.numeric)], responseVar)
    if (length(areNumeric) <= 3) {
        mfRow = c(1, length(areNumeric))
    } else {
        mfRow = c(2, ceiling(length(areNumeric)/2))
    }
    par(mfrow = mfRow)
    lapply(X = areNumeric, FUN = function(x) {
        plot(y = df[[responseVar]], x = df[[x]], col = "red", lwd = 1.5, ylab = responseVar, 
            xlab = x, main = sprintf("LS fit of %s against %s", responseVar, 
                x))
        abline(lm(as.formula(paste(responseVar, "~", x)), data = df), col = "blue", 
            lwd = 2)
    })
}
```


Here are sample plots from this function for a couple of the ISLR datasets.


```r For the mtcars dataset
library(ISLR)

data(mtcars)
plotLeastSqFit(df = mtcars, responseVar = "mpg")
```
![plot of chunk For the mtcars dataset](../images/_rmdimages/2014-06-04-eda-plotting-least-squares-fit-line-in-r/For_the_mtcars_dataset.png) 



```r For the Advertising dataset, Ch. 2, ISLR
Advertising = read.csv("http://www-bcf.usc.edu/~gareth/ISL/Advertising.csv")
Advertising[["X"]] = NULL
Advertising[["random"]] = runif(nrow(Advertising))
plotLeastSqFit(df = Advertising, responseVar = "Sales")
```
![plot of chunk For the Advertising dataset, Ch. 2, ISLR](../images/_rmdimages/2014-06-04-eda-plotting-least-squares-fit-line-in-r/For_the_Advertising_dataset__Ch__2__ISLR.png) 


<!--links-->
[islr]: http://www-bcf.usc.edu/~gareth/ISL/
