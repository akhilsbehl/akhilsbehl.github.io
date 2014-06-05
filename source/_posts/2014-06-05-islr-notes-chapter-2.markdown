---
layout: post
title: "ISLR: Notes, Chapter 2"
date: 2014-06-05 10:56:30 +0530
comments: true
categories: ISLR, notes, machine-learning
published: true
---

* Non-parametric methods seek an estimate of $f$ that gets as close to the data
  points as possible without being too _rough or wiggly_. Non-parametric
  approaches completely avoid the danger of the chosen functional form being
  too far from the true $f$. The disadvantage of non-parametric methods is that
  they need a large set of observations to obtain an accurate estimate of $f$.
  Therefore, the informational requirements of non-parametric methods are
  larger.

  Contrast this to parametric methods. Parametric methods essentially
  extrapolate information from one region of the domain to another. This is
  because global regularities are assumed in the functional form. A
  non-parametric method however has to trace the surface $f$ in all regions of
  the domain to be valid.

<!--more-->

* Almost as a rule, the flexibility of a model will trade-off it's
  interpretability. Therefore, the intent of the model, vis-a-vis prediction vs
  inference becomes the prime consideration when choosing across a spectrum of
  modeling techniques.

  However, another consideration that can affect the predictive power of a
  non-parametric model is the predisposition to overfit.

* Sometimes the question of whether an analysis should be considered supervised
  or unsupervised is less clear-cut. For instance-suppose that we have a set of
  $n$ observations. For $m$ of the observations, where $m < n$, we have both
  predictor and response measurements. Fore the remaining $n - m$ observations,
  we only have the predictors' but no response measurement. Such a scenario can
  arise if the predictors can be measured _relatively cheaply_ as compared to
  the corresponding response. We refer to this setting as a semi-supervised
  learning problem where we desire a statistical method that can handle all $n$
  observations appropriately.

* MSE:

  $$E(y_0 - \hat{f}(x_0)) = \mathrm{Var}(\hat{f}(x_0)) +
                            \mathrm{Bias}(\hat{f}(x_0)) ^ 2 +
                            \mathrm{Var}(\epsilon)$$

  The left hand side is the _expected test MSE_ that one would obtain if
  repeatedly estimating $f$ using a large number of training sets and testing
  each at $x_0$ (or averaging over a set of test values).

  Note that all three terms in the equation for MSE are non-negative.
  Therefore, it is necessarily non-negative and is bounded below by
  Var(\epsilon) which is the variance of the _irreducible_ error. We need to
  select a statistical learning method that simultaneously achieves low
  variance and low bias which objectives are always at odds with each other.

  Variance of a method refers to the amount by which $\hat{f}$ would change if
  we estimated $f$ using a different training data set. Ideally the estimate
  for $f$ should not vary too much between training sets. In general, more
  flexible statistical methods have higher variance.

  Bias refers to the error that is introduced by _approximating_ a real life
  problem, which may be complicated, by a much simpler model. Essentially, this
  is the bias in a method's prediction that is independent of the size of the
  sample available to the method. If a markedly non-linear relationship is
  approximated using a linear model, there would be residual bias no matter how
  large a training set is available to this model. As a general rule, more
  flexible methods have lower bias.

* As we choose progressively more flexible models, the variance will increase
  and the bias will decrease. The relative rate of change of these two
  quantities determines whether the test MSE increases or decreases. As we
  increase the flexibility, the bias tends to initially decrease faster than
  the variance increases. Consequently, the expected test MSE declines.
  However, at some point the increasing flexibility has little impact on the
  bias but starts to significantly increase the variance. When this happens the
  test MSE increases. [The first half of this curve is the area of innovation:
  create methods that can reduce bias faster than they increase variance.
  Consider Breiman's introduction of forests by mixing Ho's random subspace
  method and bootstrap aggregation with CARTs.]

* Look at the third panel in Fig. 2.12. Your life is so good if you are
  required to model that. Not only do you have a steep slope you will be
  descending, you have a vast plain where your MSE is virtually constant over a
  large range of parameters. You can't go very wrong there. Compare this to the
  first panel where it is a more perverse situation of blink-and-you-miss-it.

![ISLR, Fig. 2.12](../images/islr-fig-2.12.png)

* To quantify the accuracy of a **classifier** $\hat{f}$, one may use the
  **test error rate**: $$\mathrm{Ave}(I(y_0 \ne \hat{y}_0))$$.
  The test error rate is minimized (**Bayes error rate**) for the **Bayes
  classifier** that assigns each observation to the most likely class given its
  predictor values: $$\hat{y_0} = \arg\max_j Pr(Y = j | X = x_0)$$.
  The Bayes classifier produces the **Bayes decision boundary** with the
  following error rate: $$Ave(1 - \hat{y_0})$$.

  _The Bayes error rate is analogous to the irreducible error and is greater
  than zero when the categories overlap in the true population._
