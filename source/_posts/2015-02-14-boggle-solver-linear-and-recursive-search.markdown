---
layout: post
title: "Boggle solver: Linear and recursive search"
date: 2015-02-14 15:03:17 +0530
comments: true
categories: 
- python
- prefix-tree
- trie
- recursion
- recursive-search
- boggle
- scrabble
---

I was recently introduced to a Boggle solver problem by a friend of mine. Put simply, given a boggle board of sixteen characters and a dictionary, the program needs to figure out how many words from the dictionary are possible on the boggle board. The one deviation from standard boggle is that adjacent moves restriction is relaxed, i.e. order is unimportant.

My [repository][bogglerep] on Github discusses the problem and the two solutions implemented and their tradeoffs in much more detail along with presenting the code. Do visit.

<!--links-->
[bogglerep]: https://github.com/akhilsbehl/boggle-scrabble
