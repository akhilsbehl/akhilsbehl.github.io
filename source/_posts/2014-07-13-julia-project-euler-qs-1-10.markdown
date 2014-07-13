---
layout: post
title: "Julia - Project Euler Qs 1 - 10"
date: 2014-07-13 23:51:33 +0530
comments: true
categories: 
- julia
- project-euler
---

So, today I decided to take Julia for a spin because, _finally_, it has a stable version available in the [Arch](http://www.archlinux.org) community repo. I tried it with the [Project Euler problems](http://projecteuler.net/problems) to get started. Here is a bunch of code in Julia that solves problems 1 - 10 of Project Euler. I have been watching a lot of videos on the language. I will probably follow up with a post on initial thoughts about the language. 

<!--more-->

```julia
########
#  Q1  #
########

i = 1
mysum = 0
while i < 1000
  if (i % 3 == 0) || (i % 5 == 0)
    mysum += i
  end
  i += 1
end
mysum

########
#  Q2  #
########

penultimateFib = 1
ultimateFib = 2
mysum = 2
while (thisFib = penultimateFib + ultimateFib) <= 4e6
  if thisFib % 2 == 0
    mysum += thisFib
  end
  penultimateFib = ultimateFib
  ultimateFib = thisFib
end
mysum

########
#  Q3  #
########

function primep (n)
  retval = true
  for i in 2:isqrt(n)
    if n % i == 0
      retval = false
      break
    end
  end
  return(retval)
end

function nextprime(n)
  n += 1
  while (!primep(n))
    n += 1
  end
  return(n)
end

function previousprime(n)
  if n < 2
    return(2)
  end
  n -= 1
  while (!primep(n) && n > 2)
    n -= 1
  end
  return(n)
end

function primesupto(n)
  # Implements the Eratosthenes' sieve.
  sieve = repeat([true], inner=[n])
  sieve[1] = false
  for i in 2:isqrt(n)
    if sieve[i]
      sieve[2i:i:end] = false
    end
  end
  retval = Int64[]
  for i in 1:n
    if sieve[i]
      Base.push!(retval, i)
    end
  end
  return(retval)
end

# Should not be upto isqrt(n). But works for this case.
n = 600851475143
for p in reverse(primesupto(isqrt(n)))
  if n % p == 0
    println(p)
    break
  end
end

########
#  Q4  #
########

function palindromep(n)
  sn = string(n)
  return(sn == reverse(sn))
end

largest = 0
for i = 101:1:999
  for j = 101:1:999
    n = i * j
    if palindromep(n) && n > largest
      largest = n
    end
  end
end
largest

########
#  Q5  #
########

found = false
i = 380
while !found
  found = true
  i += 380
  for d in 20:-1:11
    if i % d != 0
      found = false
    end
  end
end
i

# Alternatively:
lcm([i for i in 1:20])

########
#  Q6  #
########

mysum = 0
mysumsq = 0
for i in 1:100
  mysum += i
  mysumsq += i^2
end
mysum^2 - mysumsq

########
#  Q7  #
########

i = 1
p = 2
while (i < 10001)
  p = nextprime(p)
  i += 1
end
p

########
#  Q8  #
########

num="7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"

largest = 0
for start in 1:(length(num) - 13)
  seqn = num[start:(start + 12)]
  product = prod([int(i) - 48 for i in seqn])
  if product > largest
    largest = product
  end
end
largest

########
#  Q9  #
########

# sum(25 * (8, 15, 17)) = 1000
prod([25 * i for i in [8, 15, 17]])

#########
#  Q10  #
#########

sum(primesupto(2000000 - 1))
```
