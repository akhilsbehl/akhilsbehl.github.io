#!/usr/bin/Rscript
###############################################################################
# Purpose: A script to knit Rmd documents into octopress posts and associated
#          output images.
# Author: Akhil S. Behl
# Dependencies: knitr
# Created: Mon 02 Jun 2014 02:50:03 PM IST
# Modified from: http://edild.github.io/blog/2013/03/01/knitr-octo/
###############################################################################

library(knitr)
library(stringr)

renderOcto = function (extra="") {
  render_markdown(TRUE)
  # The hook for a code chunk
  codeHook = function (x, options) {
    paste(paste("\n```r", options[["label"]]),
          paste0(x, collapse="\n"),
          "```\n", sep="\n")
  }
  # The hook for an output chunk
  outputHook = function (x, options) {
    if (knitr:::output_asis(x, options)) return(x)
    else gsub("\n{1,}$", "\n",
              gsub("## {1,2}", '',
                   str_c("\n```\n", x, "```\n")))
  }

  knit_hooks$set(source=codeHook,
                 output=outputHook,
                 warning=outputHook,
                 error=outputHook,
                 message=outputHook)

}

knit2octo = function (rmd,
                      octoPost=paste0(octoPostDir, "/", rmdName, ".markdown"),
                      octoPostDir="../_posts",
                      rmdImageDir="../_rmdimages") {
  rmdName = tools::file_path_sans_ext(basename(rmd))
  postImagesDir = paste0(rmdImageDir, "/", rmdName, "/")
  if (!file.exists(postImagesDir)) dir.create(postImagesDir)

  # Weird functions that work by side_effects. o.O
  pat_md() # Set knitr patterns to markdown.
  opts_chunk$set(fig.path=postImagesDir) # Set the images dir for the post.
  opts_knit$set(out.format='markdown')
  options(width=200) # We will be using this in a browser, let it scroll.
  renderOcto() # Set the knitting hooks specifically for octopress.

  knit(rmd, octoPost)
}

##########
#  main  #
##########

argv = commandArgs(TRUE)
if (length(argv) < 1) {
  print("knit.R /path/to/rmd_post")
  quit('no')
}
knit2octo(argv[1])
