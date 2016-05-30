---
layout: post
title: "R: Recreating the history of a stock index's membership"
date: 2013-07-15 01:40
comments: true
publish: false
categories:
- R
- Finance
- Time Series
---

If you work in finance, have you ever needed to identify the stocks that were in an index at any given date? Or perhaps a series of dates? Given how central indices are to financial markets and how frequently they change, this is a common problem.

<!--more-->

The rub is that this common problem seems too trivial till you sit down to solve it. Hopefully, this post will save you a couple of hours the next time you come across this problem (and if you are a user of R).

## Overgeneralize: Time series of membership of a set

In this post I am going to present a general method to recreate a time series of membership of a set using the current membership and a list of changes in it.

There are a few subtle points to it that we'll discuss shortly. But first let's start with a very simple example.


```r

# Let's have three time periods.
periods <- 3

# And name the change times as `t-1', `t-2', and `t-3'.
ctime <- paste0("t-", seq_len(periods))

# Define current membership of a set of alphabets.
current <- letters[1:5]

# Let's assume that "a" was added to the set at `t-1' and so on...
added <- setNames(c("a", "b", "c"), ctime)
added

# Similarly the ones that were removed:
removed <- setNames(c("f", "g", "h"), ctime)
removed

# Let's use the power of `wishful thinking' and hope we have something
# which could give us this:
create_mem_ts(ctime, added, removed, current)

$current
[1] "a" "b" "c" "d" "e"

$`t-1`
[1] "b" "c" "d" "e" "f"

$`t-2`
[1] "c" "d" "e" "f" "g"

$`t-3`
[1] "d" "e" "f" "g" "h"

attr(,"index")
[1] "t-1" "t-2" "t-3"

```


And here is what our [wishes][wishful] brought us:


```r

create_mem_ts <- function (ctime, added, removed, current) {

  stopifnot(is.atomic(ctime),
            is.atomic(added) || is.list(added),
            is.atomic(removed) || is.list(removed))

  if (any(is.na(ctime))) stop("NAs not allowed in ctime.")

  stopifnot(length(ctime) == length(added),
            length(added) == length(removed))

  if (any(duplicated(ctime))) {
    ctime.u <- unique(ctime)
    ctime.f <- factor(ctime, levels=as.character(ctime.u))
    added <- split(added, ctime.f)
    removed <- split(removed, ctime.f)
  } else {
    ctime.u <- ctime
  }

  out <- setNames(vector(mode="list", length=length(ctime.u) + 1),
                  c("current", as.character(ctime.u)))
  out[["current"]] <- current

  for (i in 2:length(out))
    out[[i]] <- union(setdiff(out[[i - 1]], added[[i - 1]]),
                      na.omit(removed[[i - 1]]))

  attr(out, "index") <- ctime.u

  out

}

```


The function is much more powerful than the simple example quoted above. Here is the [gist][gist-cmt] including this code, documentation and a few examples.

## Contextualize: Time series of stock index membership

Let's take the example of the Nifty index on the [National Stock Exchange][nse] of India.[^example] Nifty is the most important Indian stock index comprised of fifty stocks. The membership is typically shuffled twice a year. The following is the list of NSE symbols for the current members of Nifty:


```r

current_nifty

 [1] "ACC"        "AMBUJACEM"  "ASIANPAINT" "AXISBANK"   "BAJAJ-AUTO"
 [6] "BANKBARODA" "BHEL"       "BPCL"       "BHARTIARTL" "CAIRN"     
[11] "CIPLA"      "COALINDIA"  "DLF"        "DRREDDY"    "GAIL"      
[16] "GRASIM"     "HCLTECH"    "HDFCBANK"   "HEROMOTOCO" "HINDALCO"  
[21] "HINDUNILVR" "HDFC"       "ITC"        "ICICIBANK"  "IDFC"      
[26] "INDUSINDBK" "INFY"       "JPASSOCIAT" "JINDALSTEL" "KOTAKBANK" 
[31] "LT"         "LUPIN"      "M&M"        "MARUTI"     "NMDC"      
[36] "NTPC"       "ONGC"       "POWERGRID"  "PNB"        "RANBAXY"   
[41] "RELIANCE"   "RELINFRA"   "SESAGOA"    "SBIN"       "SUNPHARMA" 
[46] "TCS"        "TATAMOTORS" "TATAPOWER"  "TATASTEEL"  "ULTRACEMCO"

```


And here are the recent changes in Nifty:


```r

head(changes_in_nifty)

       ctime      added removed
1 2013-04-01 INDUSINDBK SIEMENS
2 2013-04-01       NMDC   WIPRO
3 2012-09-28      LUPIN    SAIL
4 2012-09-28 ULTRACEMCO    STER
5 2012-04-27 ASIANPAINT    RCOM
6 2012-04-27 BANKBARODA  RPOWER

```


There are two important differences between this example and the last:

- `ctime` is now a true *time class* (`POSIXt`) in R.
- There may be more than one *record* for a given `ctime`.

The function `create_mem_ts` has been designed to handle such changes (and others) seamlessly. In fact, having a true time class lends a benefit we'll soon observe.

For now, let's see how our function performs in this case:


```r

ans <- create_mem_ts(ctime=changes_in_nifty[["ctime"]],
                     added=changes_in_nifty[["added"]],
                     removed=changes_in_nifty[["removed"]]
                     current=current_nifty)

head(ans, 3)

$current
 [1] "ACC"        "AMBUJACEM"  "ASIANPAINT" "AXISBANK"   "BAJAJ-AUTO"
 [6] "BANKBARODA" "BHEL"       "BPCL"       "BHARTIARTL" "CAIRN"     
[11] "CIPLA"      "COALINDIA"  "DLF"        "DRREDDY"    "GAIL"      
[16] "GRASIM"     "HCLTECH"    "HDFCBANK"   "HEROMOTOCO" "HINDALCO"  
[21] "HINDUNILVR" "HDFC"       "ITC"        "ICICIBANK"  "IDFC"      
[26] "INDUSINDBK" "INFY"       "JPASSOCIAT" "JINDALSTEL" "KOTAKBANK" 
[31] "LT"         "LUPIN"      "M&M"        "MARUTI"     "NMDC"      
[36] "NTPC"       "ONGC"       "POWERGRID"  "PNB"        "RANBAXY"   
[41] "RELIANCE"   "RELINFRA"   "SESAGOA"    "SBIN"       "SUNPHARMA" 
[46] "TCS"        "TATAMOTORS" "TATAPOWER"  "TATASTEEL"  "ULTRACEMCO"

$`2013-04-01`
 [1] "ACC"        "AMBUJACEM"  "ASIANPAINT" "AXISBANK"   "BAJAJ-AUTO"
 [6] "BANKBARODA" "BHEL"       "BPCL"       "BHARTIARTL" "CAIRN"     
[11] "CIPLA"      "COALINDIA"  "DLF"        "DRREDDY"    "GAIL"      
[16] "GRASIM"     "HCLTECH"    "HDFCBANK"   "HEROMOTOCO" "HINDALCO"  
[21] "HINDUNILVR" "HDFC"       "ITC"        "ICICIBANK"  "IDFC"      
[26] "INFY"       "JPASSOCIAT" "JINDALSTEL" "KOTAKBANK"  "LT"        
[31] "LUPIN"      "M&M"        "MARUTI"     "NTPC"       "ONGC"      
[36] "POWERGRID"  "PNB"        "RANBAXY"    "RELIANCE"   "RELINFRA"  
[41] "SESAGOA"    "SBIN"       "SUNPHARMA"  "TCS"        "TATAMOTORS"
[46] "TATAPOWER"  "TATASTEEL"  "ULTRACEMCO" "SIEMENS"    "WIPRO"     

$`2012-09-28`
 [1] "ACC"        "AMBUJACEM"  "ASIANPAINT" "AXISBANK"   "BAJAJ-AUTO"
 [6] "BANKBARODA" "BHEL"       "BPCL"       "BHARTIARTL" "CAIRN"     
[11] "CIPLA"      "COALINDIA"  "DLF"        "DRREDDY"    "GAIL"      
[16] "GRASIM"     "HCLTECH"    "HDFCBANK"   "HEROMOTOCO" "HINDALCO"  
[21] "HINDUNILVR" "HDFC"       "ITC"        "ICICIBANK"  "IDFC"      
[26] "INFY"       "JPASSOCIAT" "JINDALSTEL" "KOTAKBANK"  "LT"        
[31] "M&M"        "MARUTI"     "NTPC"       "ONGC"       "POWERGRID" 
[36] "PNB"        "RANBAXY"    "RELIANCE"   "RELINFRA"   "SESAGOA"   
[41] "SBIN"       "SUNPHARMA"  "TCS"        "TATAMOTORS" "TATAPOWER" 
[46] "TATASTEEL"  "SIEMENS"    "WIPRO"      "SAIL"       "STER"      

```


It is easy (but boring) to check that the answer is correct. The nice part is to have a tool ready for you now, whenever you want to do this again.

## Big deal! That's just seven dates, not a timeseries.

Feeling sharp, eh Sherlock?! Drat, I wish I could create a regular time-series from the irregular time-series of changes.

Wait a second... I can! Using this:


```r

memship_at <- function (mem_ts, at) {

  stopifnot(inherits(at, class(attr(mem_ts, "index"))))

  just.before <- which(at > attr(mem_ts, "index"))[1]

  if (just.before > 1)
    mem_ts[[just.before - 1]]
  else
    mem_ts[[1]]

}

```


**Why another function? Why not just create the whole series at once?**

Because it is smart! Essentially, think of this as applying a set of diffs/patches to a text file.

- It needs less *space* to store.
- It provides the user the flexibility and efficiency (both in time and space) by computing only for the dates one asks for.
- It is a much cleaner abstraction (think initalizing and querying a database) that can work with any time-series class (or even user-written classes provided they define a *greater than* `>` method for their class).

All right! Smart, shmart, what do I do with it?

## And finally, the answer!

Use a loop or an `apply` variant to find out the membership on the dates that you want. Suppose I want the membership of Nifty on a weekly basis starting from the January 1, 2012 to March 31, 2012.

I first create a sequence of desired dates like so:


```r

my_dates <- seq.POSIXt(from=as.POSIXct("2012-01-01"),
                       to=as.POSIXct("2012-03-31"),
                       by="7 day")
 [1] "2012-01-01 IST" "2012-01-08 IST" "2012-01-15 IST" "2012-01-22 IST"
 [5] "2012-01-29 IST" "2012-02-05 IST" "2012-02-12 IST" "2012-02-19 IST"
 [9] "2012-02-26 IST" "2012-03-04 IST" "2012-03-11 IST" "2012-03-18 IST"
[13] "2012-03-25 IST"

```


Now I can use the `memship_at` function to generate membership at each of these dates like so:


```r

memship_dates <- setNames(lapply(X=my_dates, FUN=memship_at, mem_ts=ans),
                          my_dates)
head(memship_dates)
$`2012-01-01`
 [1] "ACC"        "AMBUJACEM"  "ASIANPAINT" "AXISBANK"   "BAJAJ-AUTO" "BANKBARODA"
 [7] "BHEL"       "BPCL"       "BHARTIARTL" "CAIRN"      "CIPLA"      "COALINDIA" 
[13] "DLF"        "DRREDDY"    "GAIL"       "GRASIM"     "HCLTECH"    "HDFCBANK"  
[19] "HEROMOTOCO" "HINDALCO"   "HINDUNILVR" "HDFC"       "ITC"        "ICICIBANK" 
[25] "IDFC"       "INFY"       "JPASSOCIAT" "JINDALSTEL" "KOTAKBANK"  "LT"        
[31] "M&M"        "MARUTI"     "NTPC"       "ONGC"       "POWERGRID"  "PNB"       
[37] "RANBAXY"    "RELIANCE"   "RELINFRA"   "SESAGOA"    "SBIN"       "SUNPHARMA" 
[43] "TCS"        "TATAMOTORS" "TATAPOWER"  "TATASTEEL"  "SIEMENS"    "WIPRO"     
[49] "SAIL"       "STER"      

$`2012-01-08`
 [1] "ACC"        "AMBUJACEM"  "ASIANPAINT" "AXISBANK"   "BAJAJ-AUTO" "BANKBARODA"
 [7] "BHEL"       "BPCL"       "BHARTIARTL" "CAIRN"      "CIPLA"      "COALINDIA" 
[13] "DLF"        "DRREDDY"    "GAIL"       "GRASIM"     "HCLTECH"    "HDFCBANK"  
[19] "HEROMOTOCO" "HINDALCO"   "HINDUNILVR" "HDFC"       "ITC"        "ICICIBANK" 
[25] "IDFC"       "INFY"       "JPASSOCIAT" "JINDALSTEL" "KOTAKBANK"  "LT"        
[31] "M&M"        "MARUTI"     "NTPC"       "ONGC"       "POWERGRID"  "PNB"       
[37] "RANBAXY"    "RELIANCE"   "RELINFRA"   "SESAGOA"    "SBIN"       "SUNPHARMA" 
[43] "TCS"        "TATAMOTORS" "TATAPOWER"  "TATASTEEL"  "SIEMENS"    "WIPRO"     
[49] "SAIL"       "STER"      

$`2012-01-15`
 [1] "ACC"        "AMBUJACEM"  "ASIANPAINT" "AXISBANK"   "BAJAJ-AUTO" "BANKBARODA"
 [7] "BHEL"       "BPCL"       "BHARTIARTL" "CAIRN"      "CIPLA"      "COALINDIA" 
[13] "DLF"        "DRREDDY"    "GAIL"       "GRASIM"     "HCLTECH"    "HDFCBANK"  
[19] "HEROMOTOCO" "HINDALCO"   "HINDUNILVR" "HDFC"       "ITC"        "ICICIBANK" 
[25] "IDFC"       "INFY"       "JPASSOCIAT" "JINDALSTEL" "KOTAKBANK"  "LT"        
[31] "M&M"        "MARUTI"     "NTPC"       "ONGC"       "POWERGRID"  "PNB"       
[37] "RANBAXY"    "RELIANCE"   "RELINFRA"   "SESAGOA"    "SBIN"       "SUNPHARMA" 
[43] "TCS"        "TATAMOTORS" "TATAPOWER"  "TATASTEEL"  "SIEMENS"    "WIPRO"     
[49] "SAIL"       "STER"      

```


And there we have it.

I leave the proof of correctness as an exercise for the reader. No, seriously! Let me know if you find a bug in the comments below. Feel free to clone the [gist][gist-cmt] to tinker and let me know if you improve it.

***
<!-- Footnotes -->
[^example]: The forementioned [gist][gist-cmt] contains a copy of the data used here. One may replicate this example by cloning the [git][git] repository.

<!-- Links -->
[wishful]: https://www.google.co.in/search?q=wishful+programming&oq=wishful+programming
[gist-cmt]: https://gist.github.com/akhilsbehl/6000190
[git]: http://git-scm.com
[nse]: http://nseindia.com
