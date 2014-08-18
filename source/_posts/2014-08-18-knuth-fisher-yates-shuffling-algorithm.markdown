---
layout: post
title: "Knuth-Fisher-Yates shuffling algorithm"
date: 2014-08-18 21:41:08 +0530
comments: true
categories: 
- knuth
- shuffle
- fisher-yates
- algorithms
---

So, yesterday, I was sent this [article][orgarticle] by a friend. Articles titled _X every developer should know_ sort of titles pique my interest very much. So, I headed over to this article only to find myself too confused. What I found more confusing is that the article links to [one][japost] of Jeff Atwood's blogposts that discusses the same concept.

The central idea of the post is to correct a _naive_ implementation of a shuffling algorithm. My confusion arose from the fact that after reading the article, I could only think that the algorithm was blatantly incorrect and not naive. The differences between the two algorithms -- the naive one and the one ascribed to Knuth, Fisher, & Yates is what, in sampling 101, is between sampling _with replacement_ and sampling _without replacement_. This quote essentially highlights the naivete indeed:

{% blockquote %}
How do we know that that above algorithm is biased? On the surface it seems reasonable, and certainly does some shuffling of the items.
{% endblockquote %}

Some shuffling does not mean a random permutation at all. This is akin to saying a naive implementation of a queue sometimes inserts incoming items at the head of the queue.

## Another shuffling algorithm?

On the other hand, what was fruitful was that the article reminded me of a shuffling algorithm I recently wrote to solve [problem number 25][prob25] of [99 Lisp Problems][99prob]. The minor difference between the two algorithms is that the lisp algorithm pops an element from the original array per turn and collects it in a new array which becomes a random permutation (shuffle) of the original array at the end. The KFY algorithm instead swaps two elements at each turn. It can be worked out that, mathematically, the two algorithms are identical. But the KFY algorithm saves you making a copy. Therefore, yes, KFY is an algorithm that every programmer _should_ know. Additionally, every programmer should also know the standard library of their language.

<!--links-->
[orgarticle]: http://spin.atomicobject.com/2014/08/11/fisher-yates-shuffle-randomization-algorithm/
[japost]: http://blog.codinghorror.com/the-danger-of-naivete/
[99prob]: http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html
[prob25]: http://akhilsbehl.github.io/blog/2014/06/28/99-lisp-problems-problem-number-25/
