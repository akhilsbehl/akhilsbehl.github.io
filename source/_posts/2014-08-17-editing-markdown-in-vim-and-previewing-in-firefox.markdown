---
layout: post
title: "Editing markdown in Vim and previewing in Firefox."
date: 2014-08-17 15:03:15 +0530
comments: true
categories: 
- markdown
- vim
- firefox
---

Lately, I have taken to maintaining most of my text in Markdown (desperately trying to avoid Emacs' org-mode) and previewing it while editing in Vim is something that is a nice to have. So I searched a little bit and found this nice little [snippet][originalsnippet] by Ellen Gummesson that I then adapted per my preferences as such:


```vim

" Preview markdown in Firefox

function! PreviewMarkdown()
  let outFile = expand('%:r') . '.html'
  silent execute '!cd %:p:h'
  silent execute '!python -m markdown % >' . outFile
  " The screen will need to be redrawn. Dunno why! :\
  silent execute 'redraw!'
endfunction

augroup markdown
    au!
    au BufNewFile,BufRead *.md,*.markdown setlocal filetype=ghmarkdown
    au BufNewFile,BufRead *.md,*.markdown setlocal textwidth=0
    autocmd FileType ghmarkdown map <LocalLeader>p
          \ :call PreviewMarkdown()<CR>
augroup END

```


<!--links-->
[originalsnippet]: http://ellengummesson.com/blog/2013/01/27/markdown-preview-for-vim/
