---
layout: post
title: "The corrected AIC"
date: 2014-03-22 23:35:53 +0530
comments: true
categories: 
- time series
- AIC
- statistics
- R
---

Only today I discovered that the Akaike Information Criterion is valid only
*asymptotically* and that there exists a correction (in fact, a strongly
recommended correction) for finite samples. Here is a quick copy-paste from
[Wikipedia][aicwiki].

<!--more-->

{% blockquote %}
AICc is AIC with a correction for finite sample sizes:

$$
AICc = AIC + \frac{2k(k + 1)}{n - k - 1}
$$

where n denotes the sample size. Thus, AICc is AIC with a greater penalty for
extra parameters.

Burnham & Anderson (2002) strongly recommend using AICc, rather than AIC, if n
is small or k is large. Since AICc converges to AIC as n gets large, AICc
generally should be employed regardless. Using AIC, instead of AICc, when n is
not many times larger than k2, increases the probability of selecting models
that have too many parameters, i.e. of overfitting. The probability of AIC
overfitting can be substantial, in some cases.

Brockwell & Davis (1991, p. 273) advise using AICc as the primary criterion in
selecting the orders of an ARMA model for time series. McQuarrie & Tsai (1998)
ground their high opinion of AICc on extensive simulation work with regression
and time series.

AICc was first proposed by Hurvich & Tsai (1989). Different derivations of it
are given by Brockwell & Davis (1991), Burnham & Anderson, and Cavanaugh
(1997). All the derivations assume a univariate linear model with normally
distributed errors (conditional upon regressors); if that assumption does not
hold, then the formula for AICc will usually change. Further discussion of
this, with examples of other assumptions, is given by Burnham & Anderson (2002,
ch. 7). In particular, bootstrap estimation is usually feasible.

Note that when all the models in the candidate set have the same k, then AICc
and AIC will give identical (relative) valuations. In that situation, then, AIC
can always be used.
{% endblockquote %}

It should be trivial to write this correction oneself in any language. [Here][raic1] and
[here][raic2] are a couple of such examples in R.

PS: I also learnt how to use LaTeX in octopress from [here][latexocto]. Thanks
to the author of this post.

<!--links-->
[aicwiki]: http://en.wikipedia.org/wiki/Akaike_information_criterion 
[raic1]: http://www.inside-r.org/packages/cran/sme/docs/AICc
[raic2]: http://www.awblocker.com/R/AICc.R
[latexocto]: http://www.idryman.org/blog/2012/03/10/writing-math-equations-on-octopress/
