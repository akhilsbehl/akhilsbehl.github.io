---
layout: post
title: "Inspecting objects at point with ESS"
date: 2016-05-30 13:34:32 +0530
comments: true
categories:
- R
- lisp
- emacs
- ess
---

Somewhere in the second half of last year I switched my primary text editor from [Vim][vim] to [Emacs][emacs]. Calm down! I use [Evil][evil]. So, one of my primary languages is R and I was in love with this [plugin][vimrplugin] in Vim. A snippet of my vim config that I used to rely heavily on in Vim was:

``` vim

map <LocalLeader>nr :call RAction("rownames")<CR>
map <LocalLeader>nc :call RAction("colnames")<CR>
map <LocalLeader>n2 :call RAction("names")<CR>
map <LocalLeader>nn :call RAction("dimnames")<CR>
map <LocalLeader>nd :call RAction("dim")<CR>
map <LocalLeader>nh :call RAction("head")<CR>
map <LocalLeader>nt :call RAction("tail")<CR>
map <LocalLeader>nl :call RAction("length")<CR>
map <LocalLeader>cc :call RAction("class")<CR>

```

These commands were invented by me after looking at a similar usage pattern in the plugin's manuals. Being able to inspect objects at point without switching from my editor to the R prompt made me much more productive than when I could not do this. After I switched to Emacs and the mighty [ESS][ess] for programming in R, replicating this was an explicit TODO in my ESS configuration. Ladies & gentlemen, today I bring you the solution. *Drumroll!*

<!--more-->

The primary idea --- `ess-command` wrapped with `popup-tip` --- came from [this blog post][inspiration] of yore and was wielded to magnificent effect by yours truly. (*Yes, I'm a megalomaniac, thank you very much.*)

Lemme show you *teh codez*.


``` lisp

;;; Show a popup by executing arbitrary commands on object at point.
;;; Inspiration:
;;; blogisticreflections.wordpress.com/2009/10/01/r-object-tooltips-in-ess/

;; emacs.stackexchange.com/questions/696/get-content-of-a-buffer
(defun asb-read-into-string (buffer)
  (with-current-buffer buffer
    (buffer-string)))

(defun asb-ess-R-object-popup (r-func)
  "R-FUNC: The R function to use on the object.
Run R-FUN for object at point, and display results in a popup."
  (let ((objname (current-word))
        (tmpbuf (get-buffer-create "**ess-R-object-popup**")))
    (if objname
        (progn
          (ess-command (concat "class(" objname ")\n") tmpbuf)
          (let ((bs (asb-read-into-string tmpbuf)))
            (if (not(string-match "\(object .* not found\)\|unexpected" bs))
                (progn
                  (ess-command (concat r-func "(" objname ")\n") tmpbuf)
                  (let ((bs (asb-read-into-string tmpbuf)))
                    (popup-tip bs)))))))
  (kill-buffer tmpbuf)))

(defun asb-ess-R-object-popup-str ()
  (interactive)
  (asb-ess-R-object-popup "str"))

(defun asb-ess-R-object-popup-interactive (r-func)
  (interactive "sR function to execute: ")
  (asb-ess-R-object-popup r-func))

(evil-leader/set-key-for-mode 'ess-mode "ei" 'asb-ess-R-object-popup-str)
(evil-leader/set-key-for-mode 'ess-mode "eI"
  'asb-ess-R-object-popup-interactive)

```

The function `asb-ess-R-object-popup` allows us to yank the word at point and execute an arbitrary function on it (without arguments) and display the output as a popup-tip. This function is wrapped into `asb-ess-R-object-popup-str` mapped to `<leader>ei` (**e**vil-**e**ss-**i**nspect) to inspect the `str` of the object interactively because this is what I use the most. Another function called `asb-ess-R-object-popup-interactive` mapped to `<leader>eI` (**e**vil-**e**ss-**i**nteractive-**i**nspect) asks the user what R function to inspect the object with. Common suspects will be `head`, `tail`, `names`, etc. You know the drill. Have a look at the [video][demo] and pay attention to the minibuffer.

PS: Actually, don't bother watching the video; I don't know why youtube insists on downgrading the video quality.

<!--links-->
[vim]: http://www.vim.org/
[emacs]: https://www.gnu.org/software/emacs/
[evil]: https://www.emacswiki.org/emacs/Evil
[vimrplugin]: https://github.com/vim-scripts/Vim-R-plugin
[ess]: http://ess.r-project.org/
[inspiration]: https://blogisticreflections.wordpress.com/2009/10/01/r-object-tooltips-in-ess/
[demo]: https://www.youtube.com/watch?v=JwFjWHThIy4
