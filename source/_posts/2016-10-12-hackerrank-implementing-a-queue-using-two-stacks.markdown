---
layout: post
title: "HackerRank: Implementing a queue using two stacks."
date: 2016-10-12 14:48:40 +0530
comments: true
categories: 
- hackerrank
- queue
- stack
- data-structures
- algorithms
---

The [idea](http://stackoverflow.com/questions/69192/how-to-implement-a-queue-using-two-stacks) is to keep an inbox and outbox using stacks. Here is a simple implementation.

```python

class Queue(object):

    def __init__(self):
        self._inbox = Stack()
        self._outbox = Stack()

    def _refill(self):
        if not self._outbox:
            while self._inbox:
                self._outbox.push(self._inbox.pop())

    def peek(self):
        self._refill()
        try:
            return self._outbox.peek()
        except IndexError:
            raise EmptyQueueError()

    def pop(self):
        self._refill()
        try:
            retval = self._outbox.pop()
        except IndexError:
            raise EmptyQueueError()
        else:
            return retval

    def push(self, value):
        self._inbox.push(value)


class Stack():

    def __init__(self, iterable=None):
        self._data = [] if iterable is None else list(iterable)

    def __bool__(self):
        return bool(self._data)

    def peek(self):
        return self._data[-1]

    def pop(self):
        return self._data.pop()

    def push(self, value):
        self._data.append(value)


class EmptyQueueError(BaseException):
    pass

```
