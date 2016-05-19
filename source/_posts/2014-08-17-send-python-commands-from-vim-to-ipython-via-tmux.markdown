---
layout: post
title: "Send Python commands from Vim to IPython via tmux."
date: 2014-08-17 13:58:53 +0530
comments: true
categories: 
- python
- vim
- tmux
- ipython
- screen
---

A few days ago I built a very simple framework (bunch of vimscript commands really) to work with [vim][vim] and [IPython][ipython] along the lines of the [Vim-R-Plugin][vimrplugin]. In this post I show what I had to do to get this to work.

<!--more-->

## Detour: Vim R Plugin

If you live in Vim and work with R (or hope / want to) there is *no better tool* to boost your producitivity than the marriage of these two tools as brought about in the Vim R Plugin. R was my first serious programming language (horror of horrors!) and I had gotten sold on the Vim R Plugin model of doing things very early on. The plugin provides you the whole shebang like any modern IDE such as syntax highlighting, completion based on namespaces and R environments, an object browser that opens in Vim.

However, I **don't** use any of that fancy stuff. I tend to think of myself as a _simple man with simple needs_. The only thing I use the plugin to do is to send commands from Vim to R. Additionally, I wrote my own wrappers to execute my most frequent actions on R objects under the cursor in Vim. This makes me feel like I am always in the R console even when I am in Vim. And this is just the fluency I wanted in IPython with Vim. No bells and whistles, just make me feel like my editor understands my console and that is it.

Here is the chunk from my [`.vimrc`][vimrc] that is relevant for this plugin. Even if you are not as minimalistic as I am, you may yet find the ideas in these snippets useful:


```vim

" Custom commands.
map <LocalLeader>nr :call RAction("rownames")<CR>
map <LocalLeader>nc :call RAction("colnames")<CR>
map <LocalLeader>n2 :call RAction("names")<CR>
map <LocalLeader>nn :call RAction("dimnames")<CR>
map <LocalLeader>nd :call RAction("dim")<CR>
map <LocalLeader>nh :call RAction("head")<CR>
map <LocalLeader>nt :call RAction("tail")<CR>
map <LocalLeader>nl :call RAction("length")<CR>
map <LocalLeader>cc :call RAction("class")<CR>
map <LocalLeader>nw :call SendCmdToR("system('clear')")<CR>
map <LocalLeader>ne :call SendCmdToR("system('traceback()')")<CR>
map <LocalLeader>sb :call SendCmdToR("system.time({")<CR>
map <LocalLeader>se :call SendCmdToR("})")<CR>
map <LocalLeader>tt :call SendCmdToR("tt = ")<CR>

```


The `RAction` commands work this way: vim will inspect the word under the cursor and call the specified command with that argument. For example, `,nr` (my `LocalLeader` gets mapped to `,`) gives me the `rownames` of whichever object I happen to be sitting at and so on.

The `SendCmdToR` commands are slightly different. These can be used to send arbitrary text to the R console. The last two are the most interesting to me. Whenever I want to time some arbitrary set of commands I would say `,sb` (**s**ystem.time **b**egin) which would start a `system.time({` expression block on my console. Then I send whichever R commands I want to time to the console and finish with `,se` (**s**ystem.time **e**nd) which will finish my expression and time it.

A lot of times when debugging R code I want to evaluate chunks of the body of my function / code into a temporary variable (`tt` by convention). The plugin allows me to send arbitrary visual selections from vim to R. Therefore, often I would say `,tt` to start a variable assignment expression and then send whatever chunk I want to evaluate to `tt` by selecting it visually and executing it.

# Working with IPython and Vim

I had been looking for a plugin which provides me a similar model for IPython since a long time to no avail. Things that I found either did too many things or did them too differently. I was making do with `%paste` from IPython for a long time. Even though that was usable, it always made the distinction between my editor and my console too painfully obvious. And then I revisited one of the plugins that I have known for a long time but never investigated in detail.

The [Screen][screen] plugin is a mighty lightweight and simple plugin (something that I already had as a dependency for -- wait for it -- Vim R Plugin) that allows you to send arbitrary commands from Vim running in a tmux split to an arbitrary console (think shell, lisp, ruby, whatever you want it to be). An hour of RTFM and experimentation later I had hacked out a simple usable framework. This is a preview of that. I am pretty sure I will hack it to make this more usable over time (perhaps to match my muscle memory for the Vim R Plugin). All changes should get reflected in my vimrc [here][vimrc].

This is the first time I wrote any serious VimScript. I know this is kludgy but please bear with me.


```vim

" ervandew/screen configuration to send commands to ipython.

let g:ScreenImpl = "Tmux"

" Open an ipython3 shell.
autocmd FileType python map <LocalLeader>p :ScreenShell! ipython3<CR>

" Open an ipython2 shell.
autocmd FileType python map <LocalLeader>pp :ScreenShell! ipython2<CR>

" Close whichever shell is running.
autocmd FileType python map <LocalLeader>q :ScreenQuit<CR>

" Send current line to python and move to next line.
autocmd FileType python map <LocalLeader>r V:ScreenSend<CR>j

" Send visual selection to python and move to next line.
autocmd FileType python map <LocalLeader>v :ScreenSend<CR>`>0j

" Send a carriage return line to python.
autocmd FileType python map <LocalLeader>a :call g:ScreenShellSend("\r")<CR>

" Clear screen.
autocmd FileType python map <LocalLeader>L
      \ :call g:ScreenShellSend('!clear')<CR>

" Start a time block to execute code in.
autocmd FileType python map <LocalLeader>t
      \ :call g:ScreenShellSend('%%time')<CR>

" Start a timeit block to execute code in.
autocmd FileType python map <LocalLeader>tt
      \ :call g:ScreenShellSend('%%timeit')<CR>

" Start a debugger repl to execute code in.
autocmd FileType python map <LocalLeader>db
      \ :call g:ScreenShellSend('%%debug')<CR>

" Start a profiling block to execute code in.
autocmd FileType python map <LocalLeader>pr
      \ :call g:ScreenShellSend('%%prun')<CR>

" Print the current working directory.
autocmd FileType python map <LocalLeader>gw
      \ :call g:ScreenShellSend('!pwd')<CR>

" Set working directory to current file's folder.
function SetWD()
  let wd = '!cd ' . expand('%:p:h')
  :call g:ScreenShellSend(wd)
endfunction
autocmd FileType python map <LocalLeader>sw :call SetWD()<CR>

" Get ipython help for word under cursor. Complement it with Shift + K.
function GetHelp()
  let w = expand("<cword>") . "??"
  :call g:ScreenShellSend(w)
endfunction
autocmd FileType python map <LocalLeader>h :call GetHelp()<CR>

" Get `dir` help for word under cursor.
function GetDir()
  let w = "dir(" . expand("<cword>") . ")"
  :call g:ScreenShellSend(w)
endfunction
autocmd FileType python map <LocalLeader>d :call GetDir()<CR>

" Get `dir` help for word under cursor.
function GetLen()
  let w = "len(" . expand("<cword>") . ")"
  :call g:ScreenShellSend(w)
endfunction
autocmd FileType python map <LocalLeader>l :call GetLen()<CR>


```


The `,,a` command to send just a carriage return is to complete an indented block when python expects an empty line, e.g. at the end of a `for` loop.

This opens the possibility of having a bunch of different languages that vim can understand and I can transfer my muscle memory to. I am quite excited.

<!--links-->
[vim]: http://en.wikipedia.org/wiki/Vim_%28text_editor%29
[ipython]: http://en.wikipedia.org/wiki/IPython
[vimrplugin]: https://github.com/vim-scripts/Vim-R-plugin
[vimrc]: https://github.com/akhilsbehl/configs/blob/master/vimrc
[screen]: https://github.com/ervandew/screen
