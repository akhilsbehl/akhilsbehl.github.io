---
layout: post
title: "R: dplyr vs. data.table"
date: 2014-02-13 15:10:51 +0530
comments: true
publish: true
categories:
- R
- data.table
- dplyr
- data.frame
---

[Hadley Wickham][hadley], with [Romain Francois][romain] and [Dirk Eddelbuettel][eddelbuettel], has released a new package called [dplyr][dplyr]. Once again Wickham has redesigned an important component of what one does with R. The package is designed specifically for rectangular datasets which may live in memory or databases. With the new package, Hadley also convincingly closes the efficiency gap between [plyr][plyr] and [data.table][dtable]. This new efficiency comes largely from the various C++ injections into the performance critical branches of code in `dplyr` (thanks to Romain and Eddelbuettel).

On the other hand `data.table`, a mature and popular project, started by [Matthew Dowle][dowle] has been the go-to for all performance-centric R programmers for some time now. While `dplyr` raises a serious contention to `data.table`'s claim to fame, both `data.table` and Dowle are old hands at such competition. There has long been (very healthy) competition between Hadley's `plyr`, Dowle's `data.table`, and [Wes McKinney][wes]'s `pandas` (a data munging library for Python).

In this post I add another data point to the set of benchmarks of the two packages. For the official take, see [this][bench1] and [this][bench2].

<!--more-->

## The set up

Because both `dplyr` and `data.table` have been written with efficiency for big datasets in mind, we must consider at least a moderately sized dataset for the benchmarks. Based on memory and patience I had available while writing the benchmarks, I created this synthetic dataset for the benchmarking.

```r Creating a synthetic sample
R> set.seed(pi)
R> sampSize = 1e7
R> samp = data.frame(x=runif(sampSize, 2, 4),
+                    y=rnorm(sampSize, mean=runif(1), sd=runif(1, 1, 2)),
+                    z=letters[sample.int(26, sampSize, TRUE)],
+                    w=LETTERS[sample.int(26, sampSize, TRUE)],
+                    stringsAsFactors=FALSE)
```
The following is the number of steps we want to perform to transform this sample into a (not-so-much) useful summary.

  1. Filter the samp to include only the first twenty of `letters` and the last twenty of `LETTERS`.
  2. Select only columns x, y, and z out of the data.frame.
  3. Create two new columns:
     1. xProp = x / sum(x)
     2. yScale = (y - mean(y)) / sd(y)
  4. Calculate mean(xProp) and mean(yScale) by z.
  5. Arrange / order output by z.

## Expression

```r Base R
R> baseR = samp[samp[["z"]] %in% letters[1:20] &
+               samp[["w"]] %in% LETTERS[7:26],
+               c("x", "y", "z")]
R> baseR[["xProp"]] = with(baseR, x / sum(x))
R> baseR[["yScale"]] = with(baseR, (y - mean(y)) / sd(y))
R> baseR = by(baseR, baseR[["z"]],
+             function (x)
+               c("meanXProp"=mean(x[["xProp"]]),
+                 "meanYScale"=mean(x[["yScale"]])))
R> baseR = do.call("rbind", baseR)
R> baseR
     meanXProp    meanYScale
a 1.688958e-07 -0.0031065551
b 1.690335e-07  0.0010690687
c 1.689844e-07 -0.0006084212
d 1.689843e-07 -0.0011474096
e 1.690749e-07 -0.0004805666
f 1.690463e-07  0.0028620687
g 1.690087e-07  0.0025418694
h 1.690328e-07 -0.0019483662
i 1.690802e-07 -0.0016024273
j 1.689372e-07  0.0031399568
k 1.689208e-07 -0.0026173210
l 1.690398e-07  0.0001232977
m 1.690471e-07  0.0011827109
n 1.688495e-07  0.0008292519
o 1.689488e-07  0.0019278568
p 1.690026e-07  0.0011351928
q 1.690664e-07 -0.0008938230
r 1.689304e-07 -0.0001913007
s 1.689704e-07 -0.0020868491
t 1.689081e-07 -0.0001515275
```

```r dplyr
R> dply =
+    samp %.%
+    filter(z %in% letters[1:20], w %in% LETTERS[7:26]) %.%
+    select(x, y, z) %.%
+    mutate(xProp=x / sum(x),
+           yScale=(y - mean(y)) / sd(y)) %.%
+    group_by(z) %.%
+    summarise(meanXProp=mean(xProp), meanYScale=mean(yScale)) %.%
+    arrange(z)
R> dply
   z    meanXProp    meanYScale
1  a 1.688958e-07 -0.0031065551
2  b 1.690335e-07  0.0010690687
3  c 1.689844e-07 -0.0006084212
4  d 1.689843e-07 -0.0011474096
5  e 1.690749e-07 -0.0004805666
6  f 1.690463e-07  0.0028620687
7  g 1.690087e-07  0.0025418694
8  h 1.690328e-07 -0.0019483662
9  i 1.690802e-07 -0.0016024273
10 j 1.689372e-07  0.0031399568
11 k 1.689208e-07 -0.0026173210
12 l 1.690398e-07  0.0001232977
13 m 1.690471e-07  0.0011827109
14 n 1.688495e-07  0.0008292519
15 o 1.689488e-07  0.0019278568
16 p 1.690026e-07  0.0011351928
17 q 1.690664e-07 -0.0008938230
18 r 1.689304e-07 -0.0001913007
19 s 1.689704e-07 -0.0020868491
20 t 1.689081e-07 -0.0001515275
```

```r data.table
R> dtSamp = data.table(samp)
R> dt = dtSamp[z %in% letters[1:20] & w %in% LETTERS[7:26], list(x, y, z)]
R> dt = dt[ , list(xProp=(x / sum(x)), yScale=(y - mean(y)) / sd(y), z)]
R> dt = dt[ , list(meanXProp=mean(xProp), meanYScale=mean(yScale)), by=z]
R> dt = dt[order(dt[["z"]]), ]
R> dt
    z    meanXProp    meanYScale
 1: a 1.688958e-07 -0.0031065551
 2: b 1.690335e-07  0.0010690687
 3: c 1.689844e-07 -0.0006084212
 4: d 1.689843e-07 -0.0011474096
 5: e 1.690749e-07 -0.0004805666
 6: f 1.690463e-07  0.0028620687
 7: g 1.690087e-07  0.0025418694
 8: h 1.690328e-07 -0.0019483662
 9: i 1.690802e-07 -0.0016024273
10: j 1.689372e-07  0.0031399568
11: k 1.689208e-07 -0.0026173210
12: l 1.690398e-07  0.0001232977
13: m 1.690471e-07  0.0011827109
14: n 1.688495e-07  0.0008292519
15: o 1.689488e-07  0.0019278568
16: p 1.690026e-07  0.0011351928
17: q 1.690664e-07 -0.0008938230
18: r 1.689304e-07 -0.0001913007
19: s 1.689704e-07 -0.0020868491
20: t 1.689081e-07 -0.0001515275
```

IMHO, the syntax for both `data.table` and `dplyr` is cleaner and more consistent for the kind of operations considered. But this is unsurprising because both are Domain Specific Languages (DSLs) built specificlly for this limited functionality. It is also annoying that both syntaxes confuse Vim and demand manual formatting.

Both `dplyr` and `data.table` also accept column names as free variables in expressions. Of the following three expressions,

```r
x[z %in% letters[1:20]]
x[x[["z"]] %in% letters[1:20]]
with(x, x[z %in% letters[1:20]])
```

I find the second form more elegant than the first. Although, personally, I also find the third _most correct in intent_.

If I were to judge these three on syntax, I'd probably choose `dplyr` for it's design of what is now being called a [_grammar of data manipulation_][grammar]. `data.table` on the other hand is notorious for being so far removed from traditional R (and everything else) that even advanced R users may find it tiresome. Despite the tiring nature of the new syntax, it is quite dense in expression (perhaps a little too dense). I need more experience with `dplyr` to be able to find situations where this sub-language struggles in expression.

## Efficiency benchmarks

Here is the code used to benchmark the three ways of expressing the desired data manipulation:

```r
R> set.seed(pi)
R> samp = data.frame(x=runif(1e7, 2, 4),
+                      y=rnorm(1e7, mean=runif(1), sd=runif(1, 1, 2)),
+                      z=letters[sample.int(26, 1e7, TRUE)],
+                      w=LETTERS[sample.int(26, 1e7, TRUE)],
+                      stringsAsFactors=FALSE)

# Because I think this should be out of the benchmark.
R> dtSamp = data.table(samp)

R> mbc = microbenchmark({
+    baseR = samp[samp[["z"]] %in% letters[1:20] &
+                 samp[["w"]] %in% LETTERS[7:26],
+                 c("x", "y", "z")]
+    baseR[["xProp"]] = with(baseR, x / sum(x))
+    baseR[["yScale"]] = with(baseR, (y - mean(y)) / sd(y))
+    baseR = by(baseR, baseR[["z"]],
+               function (x)
+                 c("meanXProp"=mean(x[["xProp"]]),
+                   "meanYScale"=mean(x[["yScale"]])))
+    baseR = do.call("rbind", baseR)
+  },
+  {
+    dply =
+    samp %.%
+    filter(z %in% letters[1:20], w %in% LETTERS[7:26]) %.%
+    select(x, y, z) %.%
+    mutate(xProp=x / sum(x),
+           yScale=(y - mean(y)) / sd(y)) %.%
+    group_by(z) %.%
+    summarise(meanXProp=mean(xProp), meanYScale=mean(yScale)) %.%
+    arrange(z)
+  },
+  {
+    dt = dtSamp[z %in% letters[1:20] & w %in% LETTERS[7:26], list(x, y, z)]
+    dt = dt[ , list(xProp=(x / sum(x)), yScale=(y - mean(y)) / sd(y), z)]
+    dt = dt[ , list(meanXProp=mean(xProp), meanYScale=mean(yScale)), by=z]
+    dt = dt[order(dt[["z"]]), ]
+  },
+  times=10L)

R> print(mbc) # Ignoring the expressions:
     min       lq   median       uq      max neval
7.444853 7.688928 7.735172 7.801475 7.932713    10
2.217018 2.293009 2.330963 2.425163 2.506184    10
2.847840 2.926100 2.944250 3.025527 3.071644    10
```

On a moderately big dataset and a realistic computation, both `data.table` and `dplyr` are pretty efficient and pretty similar. However, base R is not orders of magnitude worse. Tight code in base R is still quite competitive; the rub is that it takes significant effort to write tight R code.

The gap between `data.table` and `dplyr` is too small at the moment to tip the scales in anyone's favor. This essentially corroborates the official benchmarks from the `dplyr` and `data.table` communities. However, I am also looking forward to validating the claim that `data.table-1.8.11` _will push the bar higher_. Hadley has made similar comments about extending `dplyr` and making it more efficient. Another noteworthy remark is that while `data.table` is at stable version `1.8.0`, `dplyr` is at current version `0.1`. This definitely makes the latter the new cool kid on the block.

# Conclusion

Given that software shows significant switching (learning) costs and network benefits, this contest becomes very interesting. There are comparably sized `plyr` and `data.table` communities already. Both packages are different enough (from each other and base R) that they are inherently incompatible standards (though dplyr is trying to subsume data.table). The individual popularity of the two lead developers, Hadley and Dowle, may play a role. I believe that this competition will not be over soon. I am also quite certain that this competition between the incumbent and the challenger will create abundunant surplus for the users of R. Amen!

Here is the code [gist][gist].

<!--links-->
[hadley]: http://had.co.nz
[romain]: http://romainfrancois.blog.free.fr/
[eddelbuettel]: http://dirk.eddelbuettel.com/
[dplyr]: http://cran.r-project.org/web/packages/dplyr
[plyr]: http://cran.r-project.org/web/packages/plyr
[dtable]: http://cran.r-project.org/web/packages/data.table
[dowle]: http://stackoverflow.com/users/403310/matt-dowle
[wes]: http://blog.wesmckinney.com/
[bench1]: http://cran.r-project.org/web/packages/dplyr/vignettes/benchmark-baseball.html
[bench2]: http://arunsrinivasan.github.io/dplyr_benchmark/
[grammar]: http://blog.revolutionanalytics.com/2014/01/fast-and-easy-data-munging-with-dplyr.html
[gist]: https://gist.github.com/akhilsbehl/8973994
