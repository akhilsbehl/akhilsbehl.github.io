---
layout: post
title: "HackerRank: Detecting cycles in a linked list"
date: 2016-10-17 11:08:42 +0530
comments: true
categories: 
- hackerrank
- data-structures
- algorithms
- linked-lists
---

The idea is to keep a slow traverser (the tortoise) and a fast traverser (the hare). If there are no cycles, eventually, the hare should hit the end of the list. However, if there are cycles, eventually the hare will see something that the tortoise has too which is enough for us to stop traversing.


```python

class Node(object):

    def __init__(self, data = None, next_node = None):
        self.data = data
        self.next = next_node


def traverser(head, step):
    at = head
    try:
        for i in range(1, step):
            at = at.next
    except AttributeError:
        raise StopIteration
    if at.next is None:
        raise StopIteration
    yield at.next


def has_cycle(head):
    tortoise = traverser(head, 1)
    hare = traverser(head, 2)
    seen_by_tortoise = set()
    while True:
        try:
            seen_by_tortoise.add(next(tortoise))
            hare_test = next(hare)
        except StopIteration:
            return False
        else:
            if hare_test in seen_by_tortoise:
                return True

```
