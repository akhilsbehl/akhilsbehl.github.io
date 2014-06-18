---
layout: post
title: "Fisher's test of statistical significance"
date: 2014-06-18 20:54:24 +0530
comments: true
categories:
- statistics
- statistical-significance
- hypothesis-testing
- statistical-inference
- significance-testing
---

## Prologue

The past couple of days I have been reading about tests of statistical
significance and the associated maladies since I came across this
[paper][spanos] shared on a social networking site.

In [grad school][igidr] conferences, I used to hear myriad tales of caution
about p-values being unreliable, prone to misinterpretation and so on. Yet, no
econometrics prof will touch this topic in class with a barge-pole. By the
second semester, with term papers becoming the norm, the whole class was
hunting for _stars_ in the [NHFS][nhfs] wilderness. PhDs were always discussing
how they spent the whole night trying to find a model that will render the
variable of interest (on which their thesis rested) significant at 5% level. I
did too. We were taught to.

<!--more-->

I was simply too stupid then to chase this literature on my own. In the true
spirit of ['read the source, luke'][rtfs], I am going to attempt reading the
adventures of [Fisher][fisher], [Neyman][neyman], and [Pearson][pearson] in the
Royal Society. I will try to distil whatever I find interesting and post it
here. [None of this will be any original thought: it is just a spotty
statistician trying to refine his vocabulary.]

For today, I want to specifically talk about the origin of _frequentist
inference methods_ starting with Fisherian **significance testing**. Today's
material comes primarily from this [paper][gill] (excellent exposition) and the
earlier [paper][spanos] by Spanos. The [wikipedia page][wiki] is another useful
resource, especially, regarding the historical narrative.

## Test of statistical significance:

Fisher laid the groundwork for inference in the frequentist sense. As the
first-mover he built the [t-tables][tables] and decided the rules of thumb
regarding how many stars to put next to a p-value.

His stance on inductive inference was **falsificationist**, i.e. his test was
developed as a tool to _nullify_ a posited (_null_) hypothesis. Moreover, the
Fisherian test **does not** employ an _alternative_ hypothesis. A null
hypothesis typically arises as either the result of a theory or from the status
quo (conventional wisdom). In either case, we attempt to falsify the theory or
conventional wisdom based on a sample of data.

The nuance in this test becomes clearer when we consider that it is, in fact, a
**double negative**. For example:

{% pullquote %}
Suppose, we want to test if a new drug increases the longevity of cancer
patients. Conventional wisdom (or probability of chancing across such a drug)
posits that it does not, and we set up a null hypothesis to this effect. Note
that the null is in itself a negative statement. (Mathematically, we may set
this up as the difference in mean survival of subjects treated with the drug
against those treated with a placebo from the beginning of the treatment.) {"If
we can falsify the null we can infer that the new drug does not "not increase
the longevity of cancer patients". If we fail to falsify the null, we are none
the wiser."}
{% endpullquote %}

### Collecting evidence against the null hypothesis

Consider a typical test for significance for the value of the mean of a random
variable:

$$ H_0: \mu = \mu_0 = 0; X \sim \mathrm{NIID}(\mu, \sigma^2) $$

The null hypothesis is set thus because the researcher believes (on theoretical
grounds) that $\mu \ne 0$ and thus would like to collect evidence against the
null hypothesis above.

The researcher can deduce that (even before any data is collected), **assuming
the null hypothesis is true**, the following _sample statistic_ follows the
Student's $t$ distribution, i.e.:

$$ t(X_n) = \frac{\sqrt{n}(\bar{X}_n - \mu_0)}{s} \sim^{H_0} S_t(n - 1) $$

where $\bar{X}_n$ and $s^2$ are the sample mean and standard error of a random
sample $X_n$.

The researcher now sets about collecting a sample $x_n$ of size n. The mean
$$\bar{x}_n$$ may be different from 0 but since we are only working with a
_sample_, we do not know if it is so only by _chance_ (a random outcome). The
test statistic is a way of quantifying how unlikely such a chance event would
be. The more unlikely such a chance event, the stronger is our evidence against
the null hypothesis being true.

Note that evidence thus derived is never certain; always probabilistic. One
can, however, control (quantify with this test) the probability with which one
shall tolerate an error in rejecting the null. This is, essentially, the level
of significance of our test, say $\alpha$.

### t-tables and p-value

<!--$$ p(x_n) = P\[\] $$-->

Here, Fisher developed two different mechanisms to evaluate the results of his
tests, which are conceptually exactly the same but differed due to practical
consideration of the time. These are the t-tables and the p-value. The p-value
defined as:

$$ p(x_n) = P[t(X_n) \gt t(x_n); H_0] $$

is the exact probability of obtaining the test-statistic at least as extreme as
the one observed assuming $H_0$ is true. Having calculated an exact p-value one
can weigh the evidence against the null at any chosen level of significance
$\alpha$.

However, at the time, calculating exact p-values used to be difficult since
they needed to be done by hand. Fisher computed the values of test statistics
for different sample sizes ($n$) and for typical (read: arbitrarily chosen by
him) values of $\alpha$. These values were 0.5(\*), 0.01(\*\*), 0.001(\*\*\*). A
computed test statistic was then checked against these levels to determine if
it exceeded the value for a chosen level of significance.

Fisher was well aware of the arbitrariness of these levels of significance and
probably chose them for convenience in his agricultural experiments. Moreover,
now that calculation of exact p-values is cheaply available using computers,
one should always rely on reporting these exact values instead of only
reporting the legacy, arbitrary, and redundant significance stars.

### Post data statistic

The p-value is a _post-data statistic_ since it can only be computed post the
collection of data. This is a concept that becomes useful when contrasting test
of statistical significance with hypothesis testing framework à la Neyman and
Pearson.

<!--links-->
[spanos]: http://errorstatistics.files.wordpress.com/2014/05/spanos_recurring-controversies-about-p-values-and-confidence-intervals-revisited.pdf
[igidr]: http://www.igidr.ac.in
[nhfs]: http://hetv.org/india/nfhs/
[rtfs]: http://blog.codinghorror.com/learn-to-read-the-source-luke/
[fisher]: http://en.wikipedia.org/wiki/Ronald_fisher
[neyman]: http://en.wikipedia.org/wiki/Neyman
[pearson]: http://en.wikipedia.org/wiki/Egon_Pearson
[gill]: www.nyu.edu/classes/nbeck/q2/gill.pdf
[wiki]: http://en.wikipedia.org/wiki/Statistical_significance_test
[tables]: http://faculty.ksu.edu.sa/salghamdi/Statistical%20Tables/T%20Table.jpg
