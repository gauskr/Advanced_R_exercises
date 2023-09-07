
# 6 Functions -------------------------------------------------------------

# 6.1 Introduction --------------------------------------------------------

# 6.2 Function fundamentals -----------------------------------------------

# 6.2.1 Function components -----------------------------------------------

f02 <- function(x, y) {
  # A comment
  x + y
}

formals(f02)
body(f02)
environment(f02)
attr(f02, "srcref")

# 6.2.2 Primitive functions -----------------------------------------------

sum # Primitive functions calls C code directly.
`[`
typeof(sum)
typeof(`[`)
formals(sum)
body(sum)
environment(sum)

# 6.2.3 First-class functions ---------------------------------------------

f01 <- function(x) {
  sin(1 / x ^ 2)
}

lapply(mtcars, function(x) length(unique(x)))
Filter(function(x) !is.numeric(x), mtcars)
integrate(function(x) sin(x) ^ 2, 0, 1)

funs <- list(
  half = function(x) x/2,
  double = function(x) x*2
)

funs$double(10)

# 6.2.4 Invoking a function -----------------------------------------------

args <- list(1:10, na.rm = TRUE)
do.call(mean, args)

# 6.2.5 Exercises ---------------------------------------------------------

# 1. Given a name, like "mean", match.fun() lets you find a function. Given a
# function, can you find its name? Why doesn’t that make sense in R?

balls <- function(x) x*2
nuts <- function(x) x*5
coocoo <- function(x) 4/x

# Ans: While this doesn't make much sense, because if you have a function you don't
# need it's exact name - you can just make a new name and assign the function to
# that name, it's not clear that it isn't helpful sometimes. For instance, when
# collaborating with other researchers one might have agreed on names on functions
# and listed them in i a package, or script, and then want to communicate about
# the function somehow, in relation to some specific issue. While this is unlikely
# to be a problem, it's not unthinkable. So what do we do then? Is there a way to
# find the function-name? Turns out there might be. Asking ChatGPT, for instance,
# can probably solve this in some instances, if the function is publicly known.
# Here I will try to find a way of remembering the name of a specific function
# stored in the global environment.
environment(nuts)
rfrf <- mtcars
utils::edit(rfrf)
findfuncname <- function(test) {
objs <- mget(ls(pos = 1L, all = TRUE), inherits = TRUE)
funs <- Filter(is.function, objs)
bodies <- lapply(funs, body)
findfunc <- function(fun) fun == body(test)
funs[unlist(lapply(bodies, findfunc))]
}

findfuncname(function(x) 4/x) # Cool!!

# Wow! That works! And it's fun! But so extremely useless!
# If I wanted to develop this further, then it's "useful" to be able to
# recognize functions that does the same. F.i.findfuncname(x*2) should
# recognize 2*x for 1.

# 2. It’s possible (although typically not useful) to call an anonymous
# function. Which of the two approaches below is correct? Why?

function(x) 3()
(function(x) 3)
# The second one is correct. This is because of the ().
?`(`
# Effectively, ( is semantically equivalent to the identity function(x) x.
# For (, the result of evaluating the argument. This has visibility set,
# so will auto-print if used at top-level.
# That's why I have used it only as a "print output" function, when doing stuff
# like
(a <- 2*6)
semantically_equivalent <- function(x) x
semantically_equivalent(4+7)
semantically_equivalent(function(x)3)()
# So in a sense () says return x. When x is a anonympus function, it returns
# a fully operational function, equal to the function, and also prints the same
# function. At least when explicitly called as function.
semantically_equivalent(mean(c(1,5)))() # Error: attempt to apply non-function
semantically_equivalent(function(x) mean(c(1,5)))() # 3

# 3. A good rule of thumb is that an anonymous function should fit on one line
# and shouldn’t need to use {}. Review your code. Where could you have used an
# anonymous function instead of a named function? Where should you have used
# a named function instead of an anonymous function?

# I haven't really used a lot of anonymous functions. Only in exercises like this.
# The first issue, that of actually using them, is the issue to review for me.
# Sometimes I make
luniq <- function(x) unique(length(x))
# Then I do things like
lapply(mtcars, luniq)
# I guess I could just have
lapply(mtcars, function(x) unique(length(x)))
# It's a bit more typing, but it will make the code more readable for cats that
# know R-code, but not necessarily have the time to read up on my wrappers.

# 4. What function allows you to tell if an object is a function? What function
# allows you to tell if a function is a primitive function?

# is.functions allows to tell wether a object is a function.
is.function(nuts)
# is.primitive will tell you if a function is primitive.
is.primitive(`+`)

# 5. This code makes a list of all functions in the base package.

objs <- mget(ls("package:base", all = TRUE), inherits = TRUE)
funs <- Filter(is.function, objs)

# Use it to answer the following questions:

# a. Which base function has the most arguments?
library(tidyverse)
argnum <- function(x) {
  length(str_split(x, ",")[[1]])
}

arg_list <- vector("character", length(funs))

for (i in seq_along(funs)) {
arg_list[i] <- as.character(lapply(funs, args)[i])
}

number_of_arguments <- map_int(arg_list, argnum)
funs[number_of_arguments == max(number_of_arguments)]

# It seems the scan() function has the most arguments (23)

args(scan)

# function (file = "", what = double(), nmax = -1L, n = -1L, sep = "",
# quote = if (identical(sep, "\n")) "" else "'\"", dec = ".",
# skip = 0L, nlines = 0L, na.strings = "NA", flush = FALSE,
# fill = FALSE, strip.white = FALSE, quiet = FALSE, blank.lines.skip = TRUE,
# multi.line = TRUE, comment.char = "", allowEscapes = FALSE,
# fileEncoding = "", encoding = "unknown", text, skipNul = FALSE)

?scan

# scan() is a function that read data into a vector or list from the console or
# file.

# b. How many base functions have no arguments?
sum(number_of_arguments == 1)
# Seems 402 functions fits the description. I'm not going to count these.

# c. What’s special about those functions?
# Jeez... now I have to take a look. That's life I guess.

attributes(funs[number_of_arguments == 1])
is.primitive(funs$abbreviate)
sum(map_lgl(funs[number_of_arguments == 1], is.object)) # only two are
# considered objects, whatever that means.
sum(map_lgl(funs[number_of_arguments == 1], is.primitive)) # nope... that varies
funs[number_of_arguments == 1][map_lgl(funs[number_of_arguments == 1], is.object)]
# Most of them are not considered to be objects.

sum(map_lgl(funs[number_of_arguments != 1], is.object)) # only two are
# Then again, not many are... hm.

funs[number_of_arguments == 1]
?abbreviate

# d. How could you adapt the code to find all primitive functions?
# Like this!
objs <- mget(ls("package:base", all = TRUE), inherits = TRUE)
funs <- Filter(is.primitive, objs) # This changed
funs

# 6. What are the three important components of a function?
# The three important components are the names, the comments and the `{`.
# Just kidding. The three most important components are
# the body - which defines what the function does, the formals -
# which defines what parametres can be set, and the environment - which
# structures how the function finds the values associated with names.


# 7. When does printing a function not show the environment it was created in?
nuts
# When it's written in the general environment.
mean
usethis::use_template

# 6.3 Function composition ------------------------------------------------

# Population standard deviation
square <- function(x) x^2
deviation <- function(x) x - mean(x)

# Either nest
x <- runif(100)
sqrt(mean(square(deviation(x))))

# Or save intermediate results
out <- deviation(x)
out <- square(out)
out <- mean(out)
out <- sqrt(out)
out

# or use pipe (pron. "and then")
library(magrittr)
x |>
  deviation() |>
  square() |>
  mean() |>
  sqrt()

# 6.4 Lexical scoping -----------------------------------------------------

x <- 10
g01 <- function() {
  x <- 20
  x
}
g01()

# Lexical scoping has four primary rules.

# 6.4.1 Name masking ------------------------------------------------------

x <- 10
y <- 20
g02 <- function() {
  x <- 1
  y <- 2
  c(x,y)
}
g02()

x <- 2
g03 <- function() {
  y <- 1
  c(x, y)
}
g03()
y

# Run the following code in your head, then confirm the result

x <- 1
g04 <- function() {
  y <- 2
  i <- function() {
    z <- 3
    c(x, y, z)
  }
  i()
}
g04()

# 6.4.2 Functions versus variables ----------------------------------------

g07 <- function(x) x + 1
g08 <- function() {
  g07 <- function(x) x + 100
  g07(10)
}
g08()

g09 <- function(x) x + 100
g10 <- function() {
  g09 <- 10
  g09(g09)
}
g10()
# Using the same name for different things is confusing at best, and best
# avoided.

# 6.4.3 A fresh start -----------------------------------------------------

# What will happen the first time you run this function? What will happen the
# second time?39 (If you haven’t seen exists() before, it returns TRUE if
# there’s a variable with that name and returns FALSE if not.)

g11 <- function() {
  if (!exists("a")) {
    a <- 1
  } else {
    a <- a + 1
  }
  a
}
g11()
g11()

# 6.4.4 Dynamic lookup ----------------------------------------------------

g12 <- function() x + 1
x <- 15
g12()
x <- 20
g12()

codetools::findGlobals(g12)
environment(g12) <- emptyenv()
g12()

# 6.4.5 Exercises ---------------------------------------------------------

# 1. What does the following code return? Why?
# Describe how each of the three c’s is interpreted.

c <- 10
c(c = c)

# It should return 10.
# And it does!
# c <- 10 binds the name c to the value 10. It's a numeric vector of length 1.
# c() is a "combine function", which alows listing objects. One can also name
# objects, and that's what c = c does. The expression's first c give the last c
# a name-attribute. The last c in the expression is the name associated with
# 10. I'm not sure how pedagogical this is: function_c(name_c = `10_c`)
# but it illustrates the meaning of c(c = c) in the code above.

# 2. What are the four principles that govern how R looks for values?

# Firstly we have the principle of name masking. Names assign within a function
# masks names in parent environments. Then we have the function versus variables
# principle, which simply is that non-anonymous functions is treated like
# other objects, when searching for objects. Also, when searching, R separates
# between name() - a function, name - a object (and "name" - a string). Come
# to think of it, it's the last thing that is the peinciple. I guess this
# reflects "everything that exist is an object, everything that happens is a
# function". This is beautiful!

# 3. What does the following function return?
# Make a prediction before running the code yourself.

f <- function(x) {
  f <- function(x) {
    f <- function() {
      x ^ 2
    }
    f() + 1
  }
  f(x) * 2
}
f(10)

# I predict... 20.
# Crap!!! I was to hasty!

f(2)
# Should return 10
f(5) # should return 52
# And does!

# 6.5 Lazy evaluation -----------------------------------------------------

h01 <- function(x) {
  10
}
h01(stop("This is an error!"))
# x is never used, and therefore not evaluated.

# 6.5.1 Promises ----------------------------------------------------------

# Powers lazy evaluation.

# A promise has three components:

# 1. An expression.
# like x + y, which gives rise to the delayed computation.

# 2. An environment.
# Where the expression should be evaluated, i.e. the environment where the
# function is called. This makes sure that the following function returns 11,
# not 101:

y <- 10
h02 <- function(x) {
  y <- 100
  x + 1
}
h02(y)

# This also means that when you do assignment inside a call to a function,
# the variable is bound outside of the function, not inside of it.

h02(y <- 1000)
y

# 3. A value
# Which is computed and cached the first time a promise is accessed when the
# expression is evaluated in the specified environment. This ensures that the
# promise is evaluated at most once, and is why you only see “Calculating…”
# printed once in the following example.

double <- function(x) {
  message("Calculating...")
  x * 2
}
h03 <- function(x) {
  c(x, x)
}
h03(double(20))

# You cannot manipulate promises with R code. Promises are like
# a quantum state: any attempt to inspect them with R code will force an
# immediate evaluation, making the promise disappear.

# This is beautiful!!

# 6.5.2 Default arguments -------------------------------------------------

# Thanks to lazy evaluation, default values can be defined in terms of other
# arguments, or even in terms of variables defined later in the function:

h04 <- function(x = 1, y = x * 2, z = a + b) {
  a <- 10
  b <- 100
  c(x, y, z)
}
h04()

# The evaluation environment is slightly different for default and user
# supplied arguments, as default arguments are evaluated inside the function.

h05 <- function(x = ls()) {
  a <- 1
  x
}
h05()
h05(ls())

# 6.5.3 Missing arguments -------------------------------------------------

# To determine if an argument’s value comes from the user or from a default, you
# can use missing():

h06 <- function(x = 10) {
  list(missing(x), x)
}
str(h06())
str(h06(10))
# Use this sparingly.
args(sample)
# It looks like both x and size are required, but if size is not supplied,
# sample() uses missing() to provide a default.
# If I (Wickham, Gautes comm.) were to rewrite sample, I’d use an explicit NULL to indicate
# that size is not required but can be supplied:

sample <- function(x, size = NULL, replace = FALSE, prob = NULL) {
  if (is.null(size)) {
    size <- length(x)
  }

  x[sample.int(length(x), size, replace = replace, prob = prob)]
}

`%||%` <- function(lhs, rhs) {
  if (!is.null(lhs)) {
    lhs
  } else {
    rhs
  }
}

sample <- function(x, size = NULL, replace = FALSE, prob = NULL) {
  size <- size %||% length(x)
  x[sample.int(length(x), size, replace = replace, prob = prob)]
}

a <- sample(1:6, replace = T)
while (length(a) < 1000*6) {
  a <- c(a, sample(1:6, replace = T))
}
# Because of lazy evaluation, you don’t need to worry about unnecessary
# computation: the right side of %||% will only be evaluated if the left side
# is NULL.

# 6.5.4 Exercises ---------------------------------------------------------

# 1. What important property of && makes x_ok() work?

x_ok <- function(x) {
  !is.null(x) && length(x) == 1 && x > 0
}

x_ok(NULL)
x_ok(1)
x_ok(1:3)

# dunno... start w experiment:

x_ok2 <- function(x) {
  !is.null(x) & length(x) == 1 && x > 0
}

x_ok(NULL)
x_ok(1)
x_ok(1:3)
# Also works...
?`&&`
# & and && indicate logical AND... The shorter form performs elementwise
# comparisons in much the same way as arithmetic operators. The longer form
# evaluates left to right, proceeding only until the result is determined. The
# longer form is appropriate for programming control-flow and typically
# preferred in if clauses.

# `&&`, then, is faster, because it utilizes lazy evaluation. This is neat!!

# 2. What is different with this code?
# Why is this behaviour undesirable here?

x_ok <- function(x) {
  !is.null(x) & length(x) == 1 & x > 0
}

x_ok(NULL)
x_ok(1)
x_ok(1:3)

# This is undesirable if used inside a function, because it returns different
# kinds, and different dimensionalized, objects. One can not work with this.
# `&` works elementwise comparisons.

!is.null(NULL)
length(NULL) == 1
NULL > 0 # Aha! This is where the magic happens.
NULL & TRUE # logical(0)
is.logical(logical(0))
dim(logical(0)) # It has no dimensions...

x_ok(c(1:3,3:1))
x_ok(1) # This I simply don't get... now I got it. when length(x) is the
# same for all elements of x. Therefore the expression is false for all
# elements of x.

# Could it be tweaked into giving separate values for elements of vectors?
# And also fixing the different kinds of vectors-issue?

x_ok <- function(x) {
  x <- as.list(x)
  answer <- vector("logical", length(x))
  for (i in seq_along(x)) {
    answer[i] <- !is.null(x[i][[1]]) && length(x[i][[1]]) == 1 && x[i][[1]] > 0
  }
  answer
}

x_ok(list(1, c(2,3), -4, c(5, -6), 7)) # Yeah!
x_ok(-5:5)

# Yup. Depends on intention, then, what's most "useful"...


# 3. What does this function return? Why?
# Which principle does it illustrate?

f2 <- function(x = z) {
  z <- 100
  x
}
f2()
z <- 3
f2()

# This function returns 100. This is because x = z is a default argument.
# That means, unless x is defined by user of function, the function will look
# after z within the function before searching elsewhere.
# This is the principle of "default arguments", which uses the environment inside
# the function as reference, rather than looking outside. This, in turn, builds
# on lazy evaluation (it stops after the environment "born" by the function),
# and, also, I think, the name masking-principle. Name-masking is also at play,
# which, in Wickhams book, is considered a lexical scoping principle.
###################################################################

# 4. What does this function return? Why?
# Which principle does it illustrate?

y <- 10
f1 <- function(x = {y <- 1; 2}, y = 0) {
  c(x, y)
}
f1()
y

# Hm... mysterious...
?`{` # the result of the last expression evaluated.
# This has the visibility of the last evaluation.
??`;` # I haven't seen explanation of this, but it works within `{`. Seperates
# between expressions.


# 5. In hist(), the default value of xlim is range(breaks), the default
# value for breaks is "Sturges", and

range("Sturges")

# Explain how hist() works to get a correct xlim value.

# Jeez... what is he talking about...

args(hist) # NULL
?hist # this has a reference.
?nclass.Sturges # This paper is a easy read. Puh!
# Sturges takes the range and splits it according to the rule

for (i in 1:50) {
hist(rnorm(i, mean = 100, sd = 15))
} # it clearly varies depending on range and n. But it's hard to get...

nclass.Sturges # ceiling(log2(length(x)) + 1)
?ceiling
# ceiling takes a single numeric argument x and returns a numeric vector
# containing the smallest integers not less than the corresponding elements of
# x.
ceiling(log2(length(rnorm(50)))+1)
ceiling(log2(50)+1)
log2(50) # 5.64
result <- ceiling(log2(1:50)+1)
names(result) <- 1:50
result
?range
?hist
set.seed(1000)

liste <- vector("list", 20)
for (i in 31:50) {
  liste[i-30] <- list(rnorm(i, mean = 100, sd = 15))
}
liste
1:10[TRUE]
pretty
getS3method("pretty", class = "default")
?methods
pretty(liste[20][[1]], n = ceiling(log2(length(liste[20][[1]])) + 1)) # yup
hist(liste[20][[1]]) # yup
ceiling # primitive, that's written directly in C or C++. Out of my league...

# This is how hist does it:
hist(-10:10)
how_hist_finds_breakpoints <- function (x, n = ceiling(log2(length(x)) + 1), min.n = n%/%3L, shrink.sml = 0.75, high.u.bias = 1.5,
          u5.bias = 0.5 + 1.5 * high.u.bias, eps.correct = 0L, f.min = 2^-20,
          ...) {
#  x <- letters[1:5]
#  x <- x[is.finite(x <- as.numeric(x))]
#  !length(x) returns TRUE
# x # returns character(0)
  x <- x[is.finite(x <- as.numeric(x))] # returns x if x is finite
  if (!length(x))
    return(x)
##############################################################################
  z <- .Internal(pretty(min(x), max(x), n, min.n, shrink.sml,
                        c(high.u.bias, u5.bias, f.min), eps.correct, TRUE))
## .Internal(pretty etc. is hidden. Beyond my league... but this is where a
# new n is set, based on the arguments as described above.
  n <- z$n # new n! Or?
  s <- seq.int(z$l, z$u, length.out = n + 1L)
  if (!eps.correct && n) {
    delta <- diff(range(z$l, z$u)/n)
    if (any(small <- abs(s) < 1e-14 * delta))
      s[small] <- 0 # this seem to make 0 the lowest if... something. Probably
  } # to avoid negative numbers if there's only positive values.
  s # return s - which is the vector of breaks.
} # this is impossible to grasp entirely without looking "inside" R.
# And this is "outside" of what Advance R covers.
# Anyhow... it works.

# In short hist creates a theoretical desired n, using a variant of Sturges
# formula (which is a heuristic, ref. Sturges article from 1929).
# This n, is then used as input in a partly behind-the-scenes operation where
# .Internal(pretty uses arguments, some of them which varies with the n, to
# create a pretty output.

compare <- function(vec) { # This creates a quick function (not going into Shiny)
print(how_hist_finds_breakpoints(liste[vec][[1]]))
hist(liste[vec][[1]])
}

compare(1) # Amazing!

# 6. Explain why this function works. Why is it confusing?

  show_time <- function(x = stop("Error!")) {
    stop <- function(...) Sys.time()
    print(x)
  }
Sys.time("Error!")
show_time()
?stop

test <- function(x = stop("Error!")) {
  stop <- function() 3
  print(x)
}
test() # Dosen't work

test <- function(x = stop("Error!")) {
  stop <- function(...) 3
  print(x)
}
test() # This works!
??`...` # Ok... this is everywhere. Have to study the ... to get this...
# Answer. The code works "because" of the ... argument. It seem to add
# functionality to the function.
args(stop) # places ... first!
stop() # Error
stop(3) # Error: 3

## Checking the solution book... function(...) masks stop()

# 7. How many arguments are required when calling library()?
args(library)
# Only one. Everybody knows this. But why?!
library
# This is a long code. But there are three kinds of non-defined arguments which
# doesn't need userdefined args. Firstly, one (I think) logical, which defaults
# to TRUE (it might be like that with logical args?). Also some (f.i. help,
# exclude) is defined if (missing(arg)). One (include.only) simply seem to be
# interpreted as NULL. If so, then the code would be better if it used
# include.only = NULL as default.

# 6.6 ... (dot-dot-dot) ---------------------------------------------------

# In other programming languages, this type of argument is often called
# varargs (short for variable arguments), and a function that uses it is said
# to be variadic.

i01 <- function(y, z) {
  list(y = y, z = z)
}

i02 <- function(x, ...) {
  i01(...)
}

str(i02(x = 1, y = 2, z = 3))

# Using a special form, ..N, it’s possible (but rarely useful) to refer to
# elements of ... by position:

i03 <- function(...) {
  list(first = ..1, third = ..3)
}

str(i03(1, 2, 3))

# There are two primary uses of ..., both of which we’ll come back to
# later in the book:

# If your function takes a function as an argument, you want some way to pass
# additional arguments to that function. In this example, lapply() uses ... to
# pass na.rm on to mean():

x <- list(x = c(1, 3, NA), y = c(4, NA, 6))
str(lapply(x, mean, na.rm = TRUE))

# If your function is an S3 generic, you need some way to allow methods to take
# arbitrary extra arguments. For example, take the print() function. Because
# there are different options for printing depending on the type of object,
# there’s no way to pre-specify every possible argument and ... allows
# individual methods to have different arguments:

print(factor(letters), max.levels = 4)
print(y ~ x, showEnv = TRUE)

# Using ... comes with two downsides:

# When you use it to pass arguments to another function, you have to carefully
# explain to the user where those arguments go. This makes it hard to understand
# what you can do with functions like lapply() and plot().

# A misspelled argument will not raise an error. This makes it easy for typos to
# go unnoticed:

sum(1, 2, NA, na_rm = TRUE)

# 6.6.1 Exercises ---------------------------------------------------------

# 1. Explain the following results:
sum(1, 2, 3)
mean(1, 2, 3)
sum(1, 2, 3, na.omit = TRUE)
mean(1, 2, 3, na.omit = TRUE)
mean(c(1:4, 5:1, 100), trim = 0)
mean(1, 2, na.rm = 3) # hm...
mean.default
?sum
# in sum() ... refers to ... numeric or complex or logical vectors.
sum(1,2,tullball = "katt") # ERROR
sum(ape = TRUE, katten = FALSE, bongo = TRUE, bongo = TRUE) # 3
# this means logical typo arguments is recognized as numeric/integers by the
# regular corecion rules. Kind of named values, which is what they are in
c(1, 0, TRUE, bongo = TRUE)
# sum, however, is a primitive function, and is "below" regular R code,
# and implemented directly in C or C++.
mean.default
# in mean, however, ... refers to something else. Probably from the .Internal
?mean
# "further arguments passed to or from other methods."
# It clearly relates to something else. Also, the numbers touch trim and na.rm,
# but this doesn't affect results in this instance, even though they are beyond
# bounds. Have to check code to understand this.

# 2. Explain how to find the documentation for the named arguments in the
# following function call:

plot(1:10, col = "red", pch = 20, xlab = "x", col.lab = "blue")

# Why does plot(1:10, col = "red") only colour the points, not the axes or
# labels? Read the source code of plot.default() to find out.
