---
layout: post
title: "bash: quoting variables - to be or not to be"
date: 2014-03-03 14:44:16 +0530
comments: true
categories: 
- bash
- quoting
---

If you spot the bug in the following piece of code, never mind reading further.
Otherwise, go on.

```sh
list_of_archives=$(find ./archives -type f \
  -name '*.lzo' -or -name '*.gz' -or -name '*.bz2')
list_of_gzips=$(echo $list_of_archives | grep "*.gz$")
```

<!--more-->

Bash has a very intricate, yet IMHO, beautiful set of quoting rules that a
programmer needs to keep in mind. Writing a bash script, you always have three
options around quoting your variables:

* Full (strong) quoting:
* Partial (weak) quoting:
* No quoting:

Let's see a brief demo of each:

```sh
$ variable="foo bar baz"

$ echo This is a strongly quoted variable '$variable'
This is a strongly quoted $variable

$ echo This is a weakly quoted "$variable"
This is a weakly quoted foo bar baz

$ echo This is an unquoted $variable
This is an unquoted foo bar baz
```

{% pullquote %}
{"The seeming lack of difference between the partial quoting and lack of
quoting has been my favorite pet-peeve with bash. I call it a 'favorite'
because I believe that peeves such as these build a programmer's character.
"} Either that, or I am a masochist. (Another language I code often in is R.
One of the most popular references for this language is called the
[RInferno][rinferno].)
{% endpullquote %}

Essentially, these minor differences force a programmer to burn coffee at a
faster rate; learn to pay more attention; exercise anger-management etc.
improving her capability for attention to detail and concentrating on multiple
things at once.

After having been bit with this difference once again while doing something
similar to what is shown in the first snippet, I decided to exercise character
some more. My preferred reference for bash on the internet is [here][tldp].
This is what it has to say on [quoting variables][qvars].

The primary difference between partially quoted and unquoted variables in bash
is that the partial quoting `"$variable"` treates the variable as a single
string preserving all whitespace information between the tokens in the string.
Presenting a bash variable unquoted on the other hand treats each (whitespace
separated) token in the string as a (splatted?) array of arguments.

Here is a concocted example to demonstrate the difference. The output is omitted.

```sh
$ mkdir /tmp/tmp
$ touch /tmp/tmp/file.{gz,bz2,lzo}
$ list_of_archives=$(find /tmp/tmp -type f -name file.*z*)

$ list_of_gzips=$(echo "$list_of_archives" | grep "*.gz$")
$ list_of_bzips=$(echo "$list_of_archives" | grep "*.bz2$")
$ list_of_lzos=$(echo "$list_of_archives" | grep "*.lzo$")
$ echo $list_of_gzips
$ echo $list_of_bzips
$ echo $list_of_lzos

$ list_of_gzips=$(echo $list_of_archives | grep "*.gz$")
$ list_of_bzips=$(echo $list_of_archives | grep "*.bz2$")
$ list_of_lzos=$(echo $list_of_archives | grep "*.lzo$")
$ echo $list_of_gzips
$ echo $list_of_bzips
$ echo $list_of_lzos
```

A much simpler way of visualizing this difference is to use the -e flag to echo:

```sh
$ echo -e $list_of_archives
/tmp/tmp/file.lzo /tmp/tmp/file.bz2 /tmp/tmp/file.gz

$ echo -e "$list_of_archives"
/tmp/tmp/file.lzo
/tmp/tmp/file.bz2
/tmp/tmp/file.gz
```

<!--links-->
[tldp]: http://tldp.org/LDP/abs/html
[qvars]: http://tldp.org/LDP/abs/html/quotingvar.html
[rinferno]: http://www.burns-stat.com/documents/books/the-r-inferno
