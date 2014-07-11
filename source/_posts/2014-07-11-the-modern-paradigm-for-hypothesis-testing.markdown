---
layout: post
title: "The modern paradigm for hypothesis testing"
date: 2014-07-11 12:48:57 +0530
comments: true
categories: 
- statistics
- statistical-significance
- hypothesis-testing
- statistical-inference
- significance-testing
---

In a couple of earlier posts, I briefly summarized the [Fisherian][fisher] and the [Neyman-Pearson][np] (NP) approaches to frequentist hypothesis testing. In this post, I outline the modern paradigm of hypothesis testing taught in a graduate social science course and the various ways in which it is, if not wrong, misguided and prone to misinterpretation.

<!--more-->

## Background

{% pullquote %}
As discussed earlier, the Fisherian setup employs a _null_ hypothesis and a _post-data_ test statistic as evidence against the null hypothesis. Fisher leaves the setup and inference from a test upto the researcher with some guidelines. [Christensen][christensen] quotes From Fisher (1956, pp 50): 'In choosing the grounds upon which a general hypothesis should be rejected, personal judgement may and should properly be exercised. The experimenter will rightly consider all points on which, in light of current knowledge, the hypothesis may be imperfectly accurate, and will selects tests, so far as possible, sensitive to these faults, rather than to others.'

The Neyman-Pearson setup, on the other hand, employs two competing hypotheses, with a pre-data test-statistic chosen on grounds of optimal power. NP reject Fisher's _subjective_ approach and lean towards pre-specified long run error rates. Their method is essentially an objective decision theoretic framework when faced with uncertainty.

[Gill][gill] further underlines these philosophical differences in their attitudes to hypothesis testing: '{" Fisher objected to the preselection of significance level as well as the mandatory two outcome decision process. Neyman and Pearson disagreed with interpreting p-values (or worse yet, ranges of p-values indicated by "stars") as the probability of Type I errors since they do not constitute a long-range probability of rejection. Neyman and Pearson also considered the interpretation of data derived p-values to be subjective and futile."}'
{% endpullquote %}

## The modern paradigm for hypothesis testing 
Hypothesis testing, as taught in a modern graduate school, is a pragmatic straddling of both these approaches with wilful ignorance of the philosophical differences between the pioneers' approach. In a typical hypothesis test set up, one employs two competing hypothesis of which one is set up as a _null_ hypothesis and the other as an _alternative_. Again, even though these test specify a pre-specified significance level, it is the p-values instead that are used as evidence in favor of or against the null hypothesis. Therefore, such a test conveniently incorporates an alternative hypothesis sidestepping the search of a powerful test.

Let's consider the example of a linear model where a researcher wishes to establish that an exogenous variable, say IQ, has a statistically significant and positive impact on a dependent variable, say income. One will set this up in a modern test as _null_ hypothesis of $$\beta_{\mathrm{iq}} = 0$$ against an alternative hypothesis of $$\beta_{\mathrm{iq}} > 0$$. Now, even though we pre-specify a level of significance, say 95%, and an alternative hypothesis we still rely on the post-data p-value to decide whether our results are useful (even publishable) or not. In a strictly NP sense, inferences on both sides of the critical value are equally valid and still I remember being taught that 'one only rejects the null but never accepts it.' Recall that falsificationist inference is purely Fisherian. This confusion, I believe, results from the fact that most students are taught by professors who are themselves unaware of the roots of these rules of thumb.

However, awareness of the philosophies underlying the two approaches can help one move beyond the rules of thumb and choose one of the two paradigms (or others such as confidence intervals and bayesian methods etc) suitable to the problem at hand. For example, situations where the cost of a false positive far exceeds the cost of a false negative one may readily resort to Fisher's falsificationist agenda. In situations where there are clear -- theoretically identified -- dichotomies in the event space and / or the costs for the two types of errors are similar one may choose a suitably fashioned NP test. This [discussion][discussion] on stack exchange elicits some interesting insights into the choice between the two methods.

<!--links-->
[fisher]: http://akhilsbehl.github.io/blog/2014/06/18/fisher-test-of-statistical-signficance/
[np]: http://akhilsbehl.github.io/blog/2014/06/19/neyman-pearson-hypothesis-testing/
[gill]: www.nyu.edu/classes/nbeck/q2/gill.pdf
[christensen]: http://www.stat.ualberta.ca/~wiens/stat665/TAS%20-%20testing.pdf
[discussion]: http://stats.stackexchange.com/questions/23142/when-to-use-fisher-and-neyman-pearson-framework
