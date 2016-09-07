---
layout: post
title: "Toggling buffer-level hooks in Emacs"
date: 2016-09-07 18:19:05 +0530
comments: true
categories: 
- emacs
- markdown
- lisp
---

One of the formats that I like to edit is markdown (yeah, yeah, I know about org-mode, shush!). Another thing that I like to do, when editing markdown, is to preview it in my browser as html. This can be done by exporting the markdown file to html each time the markdown buffer is written to. Now, I like this solution except when I don't! To generalize the problem, one wants an action to be trigerred, perhaps for a major mode (or not), every time an event happens in a buffer. On the other hand, one also wants this behavior to be togglable at will. For example: convert all tabs to spaces, run a linter, compile a file etc every time an event occurs in the buffer.

The first sub-problem -- of triggering the action -- is solved easily using Emacs' various [hooks][hooks]. The second problem is also easily solved using a little lisp. Here's how.

<!--more-->

## Triggering an event

Let's assume I have a lisp function called `special-behavior-function` handy that knows how to execute the special behavior on a buffer. I'd like to call this every time the even happens, say, I write to the buffer. Here's how to do that:

```lisp
(add-hook 'major-mode-hook ;; plug the appropriate major mode
  (lambda ()
    (add-hook 'special-event-hook ;; plug the appropriate hook
      'special-behavior-function ;; plug the appropriate function
      nil 'local)))
```

The `'local` at the end makes sure that the hook is added local to the buffer. That's usually a good thing. Change to `nil` if you don't like it.

Now, this should be enough to solve the first subproblem. However, what if, sometimes, you want to turn this behavior off? Or having turned it off, you want to turn it back on?

## Toggling event-based behavior

To achieve this, we introduce a _variable_ to save our preference, a _wrapper function_ to check against our current preference. and a _toggle function_ to toggle this preference. (All this goes into your `.emacs`, duh!) All of this is done local to a buffer. This way, you can control the behavior for each buffer separately.

Disclaimer: this answer is _inspired_ from [this discussion][so-discussion] on SO. ;)

### Configuration variable

```lisp
(defvar inhibit-special-behavior t) ;; Change t to nil to keep on by default
```

### Wrapper function

```lisp
(defun wrap-special-behavior-function () ;; try &rest with apply if you need args
  (unless inhibit-special-behavior
    (special-behavior-function)))
```

### Toggle function

```lisp
(defun toggle-function ()
  (interactive)
  (let ((this-buffer (current-buffer)))
    (if (buffer-local-value inhibit-special-behavior this-buffer)
      (progn
        (set (make-local-variable 'inhibit-special-behavior) nil)
        (message "special behavior uninhibited."))
      (progn
        (set (make-local-variable 'inhibit-special-behavior) t)
        (message "special behavior inhibited.")))))
```

Now all you gotta do is either `M-x` this function when you need it or bind it to a key-sequence. See [this][config-emacs] and [this][config-md2html] for the markdown export example that I'm personally using.

<!--links-->

[hooks]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Standard-Hooks.html
[config-md2html]: https://github.com/akhilsbehl/configs/blob/master/scripts/md2html
[config-emacs]: https://github.com/akhilsbehl/configs/blob/master/emacs#L642
[so-discussion]: http://stackoverflow.com/questions/14913398/in-emacs-how-do-i-save-without-running-save-hooks
