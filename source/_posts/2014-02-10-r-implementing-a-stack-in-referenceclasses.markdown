---
layout: post
title: "R: Implementing a stack in ReferenceClasses"
date: 2014-02-10 10:13:35 +0530
comments: true
publish: true
categories: 
- R
- ReferenceClasses
- R5
- stack
- OOP
---

R has had a perverse but technically valid model(s) of Object Oriented
Programming (OOP) for a long time. [ReferenceClasses][refclass], introduced by
[John Chambers][jc] are the new promise of traditional OOP in R. This post
implements a stack (& queue) in R as an exercise in `ReferenceClasses` based
OOP.

<!--more-->

## The implementation


```r

stack = setRefClass(
  Class="stack",
  fields=c(
    "vec"
  ),
  methods=list(

    clear=function () {
      'Generic help string' # How come this does not work for S3?
      vec <<- vec[FALSE]
    },

    contains=function(values) {
      'Generic help string'
      return(values %in% vec)
    },

    initialize=function (vec=list(), ...) {
      vec <<- vec
      callSuper(...)
    },

    peek=function (n=1) {
      'Generic help string'
      if (n > length(vec))
        message("More elements requested than in stack.")
      return(tail(vec, n))
    },

    peekleft=function (n=1) {
      'Generic help string'
      if (n > length(vec))
        message("More elements requested than in stack.")
      return(head(vec, n))
    },

    poll = function (left=FALSE) {
      'Generic help string'
      if (length(vec) == 0L) return(NULL)
      if (left) return(popleft())
      else return(pop())
    },

    pop=function (n=1) {
      'Generic help string'
      if (n > length(vec))
        stop("More elements requested than in stack.")
      retVal = tail(vec, n)
      vec <<- head(vec, length(vec) - n)
      return(retVal)
    },

    popleft=function (n=1) {
      'Generic help string'
      if (n > length(vec))
        stop("More elements requested than in stack.")
      retVal = head(vec, n)
      vec <<- tail(vec, length(vec) - n)
      return(retVal)
    },

    push=function (value) {
      'Generic help string'
      vec <<- c(vec, value)
    },

    pushleft=function (value) {
      'Generic help string'
      vec <<- c(value, vec)
    },

    size = function () {
      'Generic help string'
      return(length(vec))
    }

  )
)

```


There! The implementation does look a lot like one would write in any pure OOP
language. The only primary difference is the use of `<<-` operator to assign
in the parent frame. There is some very interesting magic with references
going on under the hood which (to the best of my knowledge) is the only way of
obtaining pure _references_ in R. Perhaps, I will follow up with more detailed
notes on the implementation of `ReferenceClasses` using environments once I
can wrap my head around them.

## Some considerations

There are a few points that I am still chewing on at the moment:

* All members of an RC object are public members. This violates some basic
  safety contracts in OOP and may be problematic downstream with user code if
  appropriate hygiene is not practiced.

* The use of `<<-` with its associated search method may be dangerous if a
  package dev is sloppy. 

* `ReferenceClasses` pass objects by reference which is orthogonal to S3 and S4
  copy-on-write semantics. The lack of widespread understanding of RC coupled
  with inability to hide implementation noted above can create nasty surprises
  for unsuspecting users. In some way, this goes with the tradition of R
  providing another loaded gun for users to _shoot-themselves-in-the-foot_. ;)

* I was quite pleased to discover Python style docstrings provided within the
  methods. However, I was equally surprised to see them. I double checked that
  this is not, in fact, some hidden R gem that I was unaware of. (I tried using
  docstrings with S3 classes.) I would like to learn how the parser handles
  these docstrings and if (and at what cost) can we make them pervasive for all
  functions and closures regardless of the object system they exist in.

* I will probably open up `lme4` to understand how a complete package based on
  `ReferenceClasses` is implemented and what hygiene is required to use these
  in downstream code. Will post here on whatever I discover.

* There has been discussion about the space required for `ReferenceClasses`
  when they touch the disk during Serialization/Deserialization. See
  [here][serde1] and [here][serde2]. None of the discussions conclude anything
  of substance. This may be another thing that I may blog about as my
  experience grows.

## Using the freshly minted stack

Here is some code (output omitted) to take our freshly minted stack class for a
spin. I am no wiser why the contains method is not working for raw bytes.
Perhaps someone can offer an explanation in the comments.


```r

myStack = stack$new(vec=list(1L, 2L, 3L,
                             "a", "b", "c",
                             0.25, 0.5, 0.75,
                             as.raw(0L), as.raw(1L), as.raw(2L)))

str(myStack)

myStack$contains(1L)
myStack$contains("d")
myStack$contains(0.5)
myStack$contains(as.raw(seq.int(1:2))) # No clue why this shouldn't work.

myStack$peek()
myStack$peek(2)

myStack$peekleft()
myStack$peekleft(2)

myStack$pop()
myStack$pop(2)

myStack$popleft()
myStack$popleft(2)

myStack$push(as.raw(4L))
myStack$pushleft(as.raw(255L))

myStack$poll()
myStack$size()

myStack$clear()
myStack$poll()
myStack$size()

```


As always, all the code has been added as a [gist][gist] to github.

<!--links-->
[refclass]: https://stat.ethz.ch/R-manual/R-devel/library/methods/html/refClass.html
[jc]: http://en.wikipedia.org/wiki/John_Chambers_%28statistician%29
[serde1]: http://r.789695.n4.nabble.com/Reference-classes-and-memory-consumption-td4633836.html
[serde2]: https://stat.ethz.ch/pipermail/r-devel/2012-September/064776.html
[gist]: https://gist.github.com/akhilsbehl/8911837
