---
layout: post
title: "Neyman & Pearson's hypothesis testing"
date: 2014-06-19 13:32:00 +0530
comments: true
categories:
- statistics
- statistical-significance
- hypothesis-testing
- statistical-inference
- significance-testing
---

In the last [post][fisher], I talked about the test of statistical
significance. Here I talk about the next major step in frequentist inference,
i.e. Neyman Pearson hypothesis testing. I will also attempt to simultaneously
elicit the differences between the two.

<!--more-->

Neyman & Pearson (NP, hereinafter) motivate their objections to the Fisherian
technique (although they ascribe it to the mathematician [Bertrand][bertrand])
in this [paper][np33]. To quote their primary objections:

{% blockquote %}
... however small be the probability that a particular grouping of a number of stars is due to "chance," does this in itself provide any evidence of another "cause" for the grouping but "chance?" [snip] Indeed, if $x$ is a continuous variable  -- as for example is the angular distance between two stars -- then any value of $x$ is a singularity of relative probability equal to zero. We are inclined to think that as far as a particular hypothesis is concerned no test based upon the theory of probability can by itself provide any valuable evidence of the truth or falsehood of that hypothesis.
{% endblockquote %}

### Long run error probabilities

What NP did to formulate 'an efficient test of the hypothesis H' was to cast it
into a decision theoretic framework (a "rule of behavior") instead of search
for truth. Fixing the rate of errors of acceptance (Type 1 error rate) and the
rate of errors of rejection (Type 2 error rate) allows one to make objective
decisions that minimize _expected_ loss. NP motivate this thus:

{% blockquote %}
... we may search for rules to govern our behavior with regard to them, in following which we insure that, in the long run of experience, we shall not be too often wrong. Here, for example, would be such a "rule of behavior": to decide whether a hypothesis, H, of a given type be rejected or not, calculate a specified character, $x$ of the observed facts; if $x \gt x_0$ reject H, if $x \le x_0$ accept H. Such a rule tells us nothing as to whether in a particular case H is true when $x \le x_0$ or false when $x \gt x_0$. But it may often be proved that if we behave according to such a rule, then in the long run we shall reject H when it is true not more, say, than once in a hundred times, and in addition we may have evidence that we shall reject H sufficiently often when it is false.
{% endblockquote %}

While Fisher used an _achieved level of significance_ (p-value) which is a
post-data statistic, NP used the idea of fixing long run error probabilities
which can be specified before the data collection phase. More on this later.

### Two competing hypotheses

Another most important difference that NP introduced with hypothesis testing
was the introduction of two _competing_ hypotheses (**not** necessarily
christened the _null_ and the _alternative_).

Let's take the familiar example of the mean of a random variable:

$$ H_0: \mu = 0 $$  

$$ H_1: \mu > 0 $$  

$$ X \sim \mathrm{NIID}(\mu, \sigma^2) $$  

Corresponding to these hypothesis one can fix the two long run error rates
conditional on mutually exclusive events:

- Type 1 error rate ($\alpha$): the probability of rejecting $H_0$ when it is
  true; the size of the test.
- Type 2 error rate ($\beta$): the probability of failing to reject $H_0$ when
  it is false; 1 - the power of the test.

### Most efficient test

An NP hypothesis test statistic is chosen on the grounds of optimality: fixing
the size of the test the statistic **maximizes the power** of the test.

### Pre data test

Consider the simple example discussed above. The sampling distribution of Type
1 error in this case is the same as that for Fisher's t-statistic we saw in the
last post, viz.

$$ t(X_n) = \frac{\sqrt{n}(\bar{X}_n - \mu_0)}{s} \sim^{H_0} S_t(n - 1) $$

where $\bar{X}_n$ and $s^2$ are the sample mean and standard error of a random
sample $X_n$.

Given the size of the test $\alpha$ and fixing the size of the sample $n$, one
can calculate the _critical value_ of the test-statistic $c_{\alpha}$. This
completes our test before we even attempt to collect data. In this sense NP
hypothesis test is a pre-data test inasmuch as it can be fully specified before
any collection of data.

### Power calculation

To evaluate the power of the test one needs the sampling distribution of the
test statistic under $$H_1$$ (which must (?) be a [simple][simplehyp]
hypothesis such as $$\mu = \mu_1$$). This is defined as in Spanos'
[paper][spanos]:

$$ t(X_n) = \frac{\sqrt{n}(\bar{X}_n - \mu_0)}{s} \sim^{\mu = \mu_1} S_t(\delta_1, n - 1) $$

where, $\delta_1$ denotes the non-centrality parameter:

$$\delta_1 = \frac{\sqrt{n}(\mu_1 - 0)}{\sigma}$$

The power then is calculated as the following:

$$ \pi(\mu_1) = 1 - \beta(\mu_1) = P[t(x_n) > c_{\alpha}; \mu = \mu_1] $$

## Differences in practical application

Gill's [paper][gill] succintly summarises the application of the two tests in the following steps:

### Steps in a Fisherian test of significance:
1. Identify the null hypothesis.
2. Determine the appropriate test statistic and its distribution under the
   assumption that the null hypothesis is true.
3. Calculate the test-statistic from the data.
4. Determine the achieved level of significance that corresponds to the test
   statistic using the distibution under the assumption that the null
   hypothesis is true.
5. Reject $H_0$ if achieved level of significance is sufficiently small.
   Othewise reach no conclusion.

The primary motivation (in my understanding of the readings) of NP was their
discomfort with the subjectiveness in the last step above. Their claim
apparently was to provide an objective decision theoretic framework by
generalizing the rpinciples in Fisher's test of significance.

### Steps in a hypothesis test à la Neyman Pearson:
1. Identify a hypothesis of interst $$H_1$$ and a complementary hypothesis
   $$H_0$$.
2. Determine the appropriate test statistic and its distibution under the
   assumption that $$H_0$$ is true.
3. Specify a significance level ($$\alpha$$), and determine the corresponding
   critical value of the test statistic under the assumption that $$H_0$$ is
   true.
4. Calculate the test-statistic from the data.
5. Reject $$H_0$$ and accept $$H_1$$ if the test statistic is futher than the
   critical value from the expected value of the test statistic (calculated
   under the assumption that $$H_0$$ is true). Otherwise, accept $$H_0$$.

<!--links-->
[fisher]: http://akhilsbehl.github.io/blog/2014/06/18/fisher-test-of-statistical-signficance/
[np33]: http://www.stats.org.uk/statistical-inference/NeymanPearson1933.pdf
[bertrand]: http://en.wikipedia.org/wiki/Joseph_Louis_Fran%C3%A7ois_Bertrand
[spanos]: http://errorstatistics.files.wordpress.com/2014/05/spanos_recurring-controversies-about-p-values-and-confidence-intervals-revisited.pdf
[gill]: www.nyu.edu/classes/nbeck/q2/gill.pdf
[simplehyp]: http://www.emathzone.com/tutorials/basic-statistics/simple-hypothesis-and-composite-hypothesis.html
