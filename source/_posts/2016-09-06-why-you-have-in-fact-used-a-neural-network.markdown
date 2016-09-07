---
layout: post
title: "Why you have, in fact, used a neural network!"
date: 2016-09-06 17:38:12 +0530
comments: true
categories:
- statistics
- machine-learning
- neural-networks
- supervised-learning
- classification
- logistic-regression
---

Have you ever used a [logistic regression][logit-wiki]? Would you like to be able to say you have used a [neural network][nn-wiki] -- perhaps in your next interview? Here's your cheat code.

*NB: [NFL][nfl-wiki] applies insomuch as you will have to know what we are talking about in this post. This is not a tutorial.*

<!--more-->

## Logistic regression

A lot has been said and is known about the logistic regression. Refer [Chapter 11][shalizi-ada] for a thorough discussion. I'll attempt a quick recap.

The learning problem is to learn the distribution of a _categorical_ response $$Y$$ conditional on a set of covariates $$X$$, i.e. $$P[Y = i \mid X = x]$$. For the _binary_ response case, there are two values that $$Y$$ can take, e.g. $$\{\textrm{head}, \textrm{tail}\}$$, $$\{\textrm{accept}, \textrm{reject}\}$$, etc. Traditionally, one of the two responses is encoded as $$1$$ and the other $$0$$. As such, the learning problem is transformed to learning $$E[Y \mid X = x]$$ which is modeled as a [Bernoulli outcome][berndist-wiki] with probability of success parametrized as $$p(X; \theta)$$. From here, one can write the _log [likelihood][likelihood]_ under standard assumptions and maximiize it to obtain the parameter estimate $$\hat{\theta}$$. In this set up, we have still not chosen the function $$p(x; \theta)$$. Different forms of $$p$$ give rise to different classifiers. One choice -- the logistic regression -- chooses $$p$$ to be:

$$

p(x; \theta) = \frac{e^{x \bullet \theta}}{1 + e^{x \bullet \theta}}

$$

## A tale of two interpretations

Logit regression has been [reinterpreted][logit-interpretations] in innumerable ways. Heck, even this post is a rehashing of one of these interpretations. For our sake, however, we will examine two interpretations and their equivalence.

### Using the log odds-ratio

The standard intepretation coming from econometrics and the (generalized) linear models camp is the log-odds interpretation. In this interpretation, one linearly models the log of odds-ratio:

$$

\ln{\frac{P[Y = 1 \mid X = x]}{P[Y = 0 \mid X = x]}} = \ln\frac{p(x, \theta)}{1 - p(x, \theta)} = x \bullet \theta

$$

Note that rearranging this is equivalent to the defintion of $$p(x; \theta)$$ given above. However, what is more important for this discussion is to note that $$p(x; \theta)$$ can be written as a [function composition][func-comp] $$\sigma \bullet$$ applied on the input vector of covariates ($$x$$) and the parameter vector to be learned ($$\theta$$). Here $$\sigma$$ is the logistic function and $$\bullet$$ is the dot product of two vectors.

### Using a log-linear model

Another interpretation of a logistic regression is as a [log-linear][loglin-wiki] model. The difficulty is that the log-linear method is used to estimate outcomes which have support in $$[0, \infty)$$ whereas we need to model $$p$$ which is a probability distribution with support in [0, 1]. The trick to overcome this is to model $$\tilde{p}$$ -- an _un-normalized_ probability distribution for each of the binary outcomes as a log-linear model. The true probability distribution $$p$$ is then realized from $$\tilde{p}$$ by normalizing it over all outcomes.

$$

\begin{align}
\ln{\tilde{p}[Y = 0 \mid X = x]} = x \bullet \beta_0 \\
\ln{\tilde{p}[Y = 1 \mid X = x]} = x \bullet \beta_1 \\
\end{align}

$$

Expnonentiating and summing the two _un-normalized_ probabilities, we get (say) $$Z = e^{x \bullet \beta_0} + e^{x \bullet \beta_1}$$ which we use to find a _normalized_ probability distribution:

$$

\begin{align}
P[Y = 0 \mid X = x] = \frac{1}{Z}e^{x \bullet \beta_0} = \frac{e^{x \bullet \beta_0}}{e^{x \bullet \beta_0} + e^{x \bullet \beta_1}} \\
P[Y = 1 \mid X = x] = \frac{1}{Z}e^{x \bullet \beta_1} = \frac{e^{x \bullet \beta_1}}{e^{x \bullet \beta_0} + e^{x \bullet \beta_1}}
\end{align}

$$

Note that in this formulation, I use $$\beta$$ to denote model parameters and not $$\theta$$. This is because the parameter estimates are in fact different. Contrast the probability of _success_ across the two interpretations:

$$

P[Y = 1 \mid X = x] = \frac{e^{x \bullet \beta_1}}{e^{x \bullet \beta_0} + e^{x \bullet \beta_1}} = \frac{e^{x \bullet \theta}}{1 + e^{x \bullet \theta}} \\

$$

This arises out of the fact that the log-linear formulation is [_over-identified_][par-ident-wiki] because if we know $$P[Y = 1 \mid X = x]$$, we automatically know $$P[Y = 0 \mid X = x]$$. The over-identification is resolved by setting $$\beta_0 = 0$$ which reconciles the two expressions above. See [the wiki][loglin-wiki] for an explanation.

Once again, we notice that even this formulation of the logistic regression can be factorized into a function composition where $$P[Y = i \mid X = x] \equiv p_i(x; \theta) \equiv s \bullet$$ where $$s$$ is the [softmax][softmax-wiki] function and $$\bullet$$ is the dot product.

##### Generalization to the multinomial case with the softmax function

The other advantage to choosing the log-linear formulation is that it is easily generalized to the case of the _n-ary_ categorical variable -- equivalent to the _multinomial_ logit case. One can easily replicate the discussion above to derive:

$$

P[Y = i \mid X = x] \equiv p_i(x; \theta) = \frac{1}{Z}e^{x \bullet \beta_i} = \frac{e^{x \bullet \beta_i}}{\sum_{k} e^{x \bullet \beta_k}}

$$

## NNs as function composition machinery

The class of neural networks that we are going to use here is the simple feed-forward multi-layer perceptron ([MLP][mlp-wiki]). For a quick recap, a multi layer perceptron is a directed acyclical graph which has each layer fully-connected to the next layer. The simplest MLP will have at least three layers: the _input_ layer (to which the data is fed), an _output_ layer (which outputs the results) and one (or more for deeper architectures) _hidden_ layer. [Goodfellow et al.][goodfellow-book] (Ch. 1, p. 5) describe:

{% blockquote %}

A multilayer perceptron is just a mathematical function mapping some set of input values to output values. The function is formed by <strong>composing many simpler functions</strong>. We can think of each application of a diﬀerent mathematical function as providing a new representation of the input.

{% endblockquote %}

Emphasis added.

In fact, in Figure 1.3, the book provides the MLP graph that represents logistic regression (as the composition $$\sigma \bullet$$, further decomposing $$\bullet$$ into elementary operations $$\times$$ and $$+$$).

### MLP design of a multinomial logit

Similarly, the log-linear interpretation of the logistic regression model can be used to design an n-ary MLP classifier with the following layers:

1. Input layer of covariates.
2. Fully connected hidden layer with n _linear units_ (the $$\bullet$$ function).
3. Fully connected output layer with n _softmax units_ (the $$s$$ function).

This here is the blueprint of your cheat code promised in the beginning of the post! ;) See this [lecture][freitas-lecture] for a full exposition on this design.

## tl;dr

This is the key insight that motivated this article: NNs are arbitrary function composition machinery that can be tuned to learn model parameters. This was illustrated by decomposing the multinomial logistic regression as a (quite _shallow_, in fact) composition of functions and then re-assembling it as a MLP. Indeed, [Bengio][ftml-book] (Ch. 2, pp. 15 - 16) summarizes that a diverse set of (machine?) learning techniques such as GLM, kernel machines, decision trees, boosting machines, stacking, and even the human cortex etc. are networks with increasingly complex or _deep_ (&agrave; la _deep learning_) compositions of simple functions.

<!--links-->
[logit-wiki]: https://en.wikipedia.org/wiki/Logistic_regression
[nn-wiki]: https://en.wikipedia.org/wiki/Artificial_neural_network
[nfl-wiki]: https://en.wikipedia.org/wiki/There_ain%27t_no_such_thing_as_a_free_lunch
[berndist-wiki]: https://en.wikipedia.org/wiki/Bernoulli_distribution
[shalizi-ada]: http://www.stat.cmu.edu/~cshalizi/ADAfaEPoV/ADAfaEPoV.pdf
[likelihood]: https://en.wikipedia.org/wiki/Likelihood_function
[logit-interpretations]: https://en.wikipedia.org/wiki/Logistic_regression#Formal_mathematical_specification
[func-comp]: https://en.wikipedia.org/wiki/Function_composition
[loglin-wiki]: https://en.wikipedia.org/wiki/Log-linear_model
[par-ident-wiki]: https://en.wikipedia.org/wiki/Parameter_identification_problem
[loglin-wiki]: https://en.wikipedia.org/wiki/Logistic_regression#As_a_.22log-linear.22_model
[softmax-wiki]: https://en.wikipedia.org/wiki/Softmax_function
[mlp-wiki]: https://en.wikipedia.org/wiki/Multilayer_perceptron
[goodfellow-book]: http://www.deeplearningbook.org/
[ftml-book]: http://www.iro.umontreal.ca/~bengioy/papers/ftml_book.pdf
[freitas-lecture]: https://www.youtube.com/watch?v=FYgsztDxSvE
