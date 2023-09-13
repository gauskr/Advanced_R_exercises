
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
# It clearly relates to something else. Also, the numbers touch trim
# and na.rm, but this doesn't affect results in this instance, even
# though they are beyond bounds. Have to check code to understand
# this.

# 2. Explain how to find the documentation for the named arguments
# in the following function call:

plot(1:10, col = "red", pch = 20, xlab = "x", col.lab = "blue")
?plot
toy_plot <- function(col, pch, xlab, col.lab) {
  plot(1:10, col = "red", pch = 20, xlab = "x", col.lab = "blue")
}
# Ans. to find documentation for these named arguments (col, pch, xlab,
# col.lab), one can either click via plot(), or write
?`graphical parameter`
# to find the help page from par{graphics} called
# "Set or Query Graphical Parameters".

# col is A specification for the default plotting color. One can see
# section ‘Color Specification’, in the same document,
# for further details.
# Some functions such as lines and text accept a vector of values
# which are recycled and may be interpreted slightly differently.
# Colors can be specified in several different ways. The simplest
# way is with a character string giving the color name
# (e.g., "red"), as in the example above. A list of the possible
# colors can be obtained with the function colors. Alternatively,
# colors can be specified directly in terms of their RGB components
# with a string of the form "#RRGGBB" where each of the pairs RR,
# GG, BB consist of two hexadecimal digits giving a value in the
# range 00 to FF. Colors can also be specified by giving an index
# into a small table of colors, the palette: indices wrap round so
# with the default palette of size 8, 10 is the same as 2. This
# provides compatibility with S. Index 0 corresponds to the
# background color. Note that the palette (apart from 0 which is
# per-device) is a per-session setting.
colors() # this gives a list of 657 colors to choose from.
# Thees apply to col.lab as well
# col.lab
# The color to be used for x and y labels. Defaults to "black".

toy_plot(col = "wheat")
plot(1:10, col = "wheat3", pch = 20, xlab = "x", col.lab = "yellow4")
plot(1:10, col = "#110199", pch = 20, xlab = "x", col.lab = "#975321")

# pch
# is also a graphical parameter, with documentation at the same place
# /page as col and col.lab.
# pch should be either an integer specifying a symbol or a single
# character to be used as the default in plotting points. See points
# for possible values and their interpretation. And note that only
# integers and single-character strings can be set as a graphics
# parameter (and not NA nor NULL). Some functions such as points
# accept a vector of values which are recycled
?points()
# there are 25 different shapes for point, here's a few examples:
plot(1:10, col = "skyblue2", pch = 11, xlab = "x", col.lab = "green3")
# 11 is combined with skyblue2 is kosher while
plot(1:10, col = "red4", pch = 23, xlab = "x", col.lab = "#975321")
# ain't square.

# The above arguments can be found via graphical parameters, as
# described, but xlab is found directly in ?plot:
# xlab
# a title for the x axis: see title.
?title
# title, however, is also from graphics:
# xlab
# X axis label using font, size and color par(c("font.lab",
# "cex.lab", "col.lab")).
# Above we already saw the col.lab - which changes the color of
# x lab (the same applies to ylab). One could also change font and
# size, using font.lab and cex.lab
??font.lab

# 3. Why does plot(1:10, col = "red") only colour the points, not
# the axes or labels? Read the source code of plot.default() to find
# out.

plot(1:10, col = "red")
?plot.default
# Well it does now...
# Inside the code plot() does
xlabel <- if (!missing(x)) # This is why we HAVE x-label!
deparse1(substitute(x))
# It does something similar for the y-axis. Will consult solutions
# book:

# To learn about the internals of plot.default() we add browser() to
# the first line of the code and interactively run
# plot(1:10, col = "red"). This way we can see how the plot is built
# and learn where the axes are added.


new_plot <- function (x, y = NULL, type = "p", xlim = NULL, ylim = NULL,
          log = "", main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
          ann = par("ann"), axes = TRUE, frame.plot = axes, panel.first = NULL,
          panel.last = NULL, asp = NA, xgap.axis = NA, ygap.axis = NA,
          ...)
{
  browser()
  localAxis <- function(..., col, bg, pch, cex, lty, lwd) Axis(...)
  localBox <- function(..., col, bg, pch, cex, lty, lwd) box(...)
  localWindow <- function(..., col, bg, pch, cex, lty, lwd) plot.window(...)
  localTitle <- function(..., col, bg, pch, cex, lty, lwd) title(...)
  xlabel <- if (!missing(x))
    deparse1(substitute(x))
  ylabel <- if (!missing(y))
    deparse1(substitute(y))
  xy <- xy.coords(x, y, xlabel, ylabel, log)
  xlab <- if (is.null(xlab))
    xy$xlab
  else xlab
  ylab <- if (is.null(ylab))
    xy$ylab
  else ylab
  xlim <- if (is.null(xlim))
    range(xy$x[is.finite(xy$x)])
  else xlim
  ylim <- if (is.null(ylim))
    range(xy$y[is.finite(xy$y)])
  else ylim
  dev.hold()
  on.exit(dev.flush())
  plot.new()
  localWindow(xlim, ylim, log, asp, ...)
  panel.first
  plot.xy(xy, type, ...)
  panel.last
  if (axes) {
    localAxis(if (is.null(y))
      xy$x
      else x, side = 1, gap.axis = xgap.axis, ...)
    localAxis(if (is.null(y))
      x
      else y, side = 2, gap.axis = ygap.axis, ...)
  }
  if (frame.plot)
    localBox(...)
  if (ann)
    localTitle(main = main, sub = sub, xlab = xlab, ylab = ylab,
               ...)
  invisible()
}
new_plot(1:10, col = "red")
# Ok, so I misinterpreted the question! Working late, long and hard...
?title
title
# The call to localTitle() passes the col parameter as part of
# the ... argument to title(). ?title tells us that the title()
# function specifies four parts of the plot: Main (title of the
# plot), sub (sub-title of the plot) and both axis labels.
# Therefore, it would introduce ambiguity inside title() to use col
# directly. Instead, one has the option to supply col via the ...
# argument, via col.lab or as part of xlab in the form
# xlab = list(c("index"), col = "red") (similar for ylab).

# 6.7 Exiting a function --------------------------------------------------

# 6.7.1 Implicit versus explicit returns ----------------------------------

# There are two ways that a function can return a value:

# Implicitly, where the last evaluated expression is the return
# value:

j01 <- function(x) {
  if (x < 10) {
    0
  } else {
    10
  }
}
j01(5)
j01(15)

# Explicitly, by calling return():

j02 <- function(x) {
  if (x < 10) {
    return(0)
  } else {
    return(10)
  }
}

# 6.7.2 Invisible values --------------------------------------------------

# Most functions return visibly: calling the function in an
# interactive context prints the result.

j03 <- function() 1
j03()

# However, you can prevent automatic printing by applying
# invisible() to the last value:

j04 <- function() invisible(1)
j04()
print(j04())
(j04())
withVisible(j04())

# The most common function that returns invisibly is <-:
a <- 2
(a <- 2)
# This is what makes it possible to chain assignments:
a <- b <- c <- d <- 2

# In general, any function called primarily for a side effect
# (like <-, print(), or plot()) should return an invisible value
# (typically the value of the first argument).

# 6.7.3 Errors

# If a function cannot complete its assigned task, it should throw
# an error with stop(), which immediately terminates the execution
# of the function.

j05 <- function() {
  stop("I'm an error!")
  return(10)
}
j05()

# An error indicates that something has gone wrong, and forces the
# user to deal with the problem. Some languages
# (like C, Go, and Rust) rely on special return values to indicate
# problems, but in R you should always throw an error. You’ll learn
# more about errors, and how to handle them, in Chapter 8.

# 6.7.4 Exit handlers -----------------------------------------------------

# Sometimes a function needs to make temporary changes to the
# global state. But having to cleanup those changes can be painful
# (what happens if there’s an error?). To ensure that these changes
# are undone and that the global state is restored no matter how a
# function exits, use on.exit() to set up an exit handler. The
# following simple example shows that the exit handler is run
# regardless of whether the function exits normally or with an
# error.

j06 <- function(x) {
  cat("Hello\n")
  on.exit(cat("Goodbye!\n"), add = TRUE)
  if (x) {
    10
  } else {
    stop("Error")
  }
}

j06(TRUE)
j06(FALSE)

# Always set add = TRUE when using on.exit(). If you don’t, each
# call to on.exit() will overwrite the previous exit handler. Even
# when only registering a single handler, it’s good practice to set
# add = TRUE so that you won’t get any unpleasant surprises if you
# later add more exit handlers.

# on.exit() is useful because it allows you to place clean-up code
# directly next to the code that requires clean-up:

cleanup <- function(dir, code) {
  old_dir <- setwd(dir)
  on.exit(setwd(old_dir), add = TRUE)

  old_opt <- options(stringsAsFactors = FALSE)
  on.exit(options(old_opt), add = TRUE)
}

# Coupled with lazy evaluation, this creates a very useful pattern
# for running a block of code in an altered environment:

with_dir <- function(dir, code) {
  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE)

  force(code)
}

getwd()
with_dir("~", getwd()) # good thing that didn't alter the
# working environment! Thank you with_dir!

# The use of force() isn’t strictly necessary here as simply
# referring to code will force its evaluation. However, using
# force() makes it very clear that we are deliberately forcing the
# execution. You’ll learn other uses of force() in Chapter 10.

# The withr package41 provides a collection of other functions for
# setting up a temporary state.

# In R 3.4 and earlier, on.exit() expressions are always run in
# order of creation:

j08 <- function() {
  on.exit(message("a"), add = TRUE)
  on.exit(message("b"), add = TRUE)
}
j08()

# This can make cleanup a little tricky if some actions need to
# happen in a specific order; typically you want the most recent
# added expression to be run first. In R 3.5 and later, you can
# control this by setting after = FALSE:

j09 <- function() {
  on.exit(message("a"), add = TRUE, after = FALSE)
  on.exit(message("b"), add = TRUE, after = FALSE)
}
j09()

# 6.7.5 Exercises ---------------------------------------------------------

# 1. What does load() return? Why don’t you normally see these
# values?

?load

## save all data
xx <- pi # to ensure there is some data
save(list = ls(all.names = TRUE), file= "all.rda")
rm(xx)
xx
## restore the saved values to the current environment
local({
  load("all.rda")
  ls()
})

xx <- exp(1:3)
## restore the saved values to the user's workspace
load("all.rda") ## which is here *equivalent* to
## load("all.rda", .GlobalEnv)
## This however annihilates all objects in .GlobalEnv with the same names !
xx # no longer exp(1:3)
rm(xx)
attach("all.rda") # safer and will warn about masked objects w/ same name in .GlobalEnv
ls(pos = 1)
xx
##  also typically need to cleanup the search path:
detach("file:all.rda")

## clean up (the example):
unlink("all.rda")

# load(), then, loads R-data into an environment.
?readRDS()
readRDS

new_load <- function (file, envir = parent.frame(), verbose = FALSE)
{
  browser()
  if (is.character(file)) {
    con <- gzfile(file)
    on.exit(close(con))
    magic <- readChar(con, 5L, useBytes = TRUE)
    if (!length(magic))
      stop("empty (zero-byte) input file")
    if (!grepl("RD[ABX][2-9]\n", magic)) {
      if (grepl("RD[ABX][2-9]\r", magic))
        stop("input has been corrupted, with LF replaced by CR")
      warning(sprintf("file %s has magic number '%s'\n",
                      sQuote(basename(file)), gsub("[\n\r]*", "", magic)),
              "  ", "Use of save versions prior to 2 is deprecated",
              domain = NA, call. = FALSE)
      return(.Internal(load(file, envir)))
    }
  }
  else if (inherits(file, "connection")) {
    con <- if (inherits(file, "gzfile") || inherits(file,
                                                    "gzcon"))
      file
    else gzcon(file)
  }
  else stop("bad 'file' argument")
  if (verbose)
    cat("Loading objects:\n")
  .Internal(loadFromConn2(con, envir, verbose))
}

new_load("all.rda")
rm("new_load")
?readChar
?gzfile
## Hm... don't get it..
# returns to ?load...

# There it is:

# Value
# A character vector of the names of objects created, invisibly.

# So.. the answer is load() creates R-objects from a file etc.
# We don't see these values because the vector of object-names
# is created invisibly. Just like `<-`. I don't understand how,
# but it might have something to do with what's going on in the
# internernals, since load is built around a corresponding internal
# function.

# 2. What does write.table() return? What would be more useful?
?write.table
# write.table prints its required argument x (after converting it
# to a data frame if it is not one nor a matrix) to a file or
# connection.

# Since we have ways to create dataframes, it would be more useful
# if write.table didn't force this? But instead returned an error?
# Is my humble guess.
# Let's check the solutions!

# A: write.table() writes an object, usually a data frame or a
# matrix, to disk. The function invisibly returns NULL. It would be
# more useful if write.table() would (invisibly) return the input
# data, x. This would allow to save intermediate results and
# directly take on further processing steps without breaking the
# flow of the code (i.e. breaking it into different lines). One
# package which uses this pattern is the {readr} package,12 which
# is part of the tidyverse-ecosystem.

# Ok... so... I could have gotten a better answer by trying to write
# something and put it into an object.

test <- write.table(1:10, "stupid.txt")
test
unlink("stupid.txt")

# 3. How does the chdir parameter of source() compare to with_dir()?
# Why might you prefer one to the other?

?source
source

# source() includes this snippet:
owd <- getwd()
if (is.null(owd))
  stop("cannot 'chdir' as current directory is unknown")
on.exit(setwd(owd), add = TRUE)

# This seem risky, if it's a new code/function. By doing stop
# before on.exit, as source() does, one risk doing harm before
# making it ok. One could also set this at the beginning of the
# script. That sounds like a better pattern to follow. Let's check
# the solutions...

# with_dir() takes a path for a working directory (dir) as its
# first argument. This is the directory where the provided code
# (code) should be executed. Therefore, the current working
# directory is changed in with_dir() via setwd(). Then, on.exit()
# ensures that the modification of the working directory is reset
# to the initial value when the function exits. By passing the path
# explicitly, the user has full control over the directory to
# execute the code in.
# In source() the code is passed via the file argument (a path to a
# file). The chdir argument specifies if the working directory
# should be changed to the directory containing the file. The
# default for chdir is FALSE, so you don’t have to provide a value.
# However, as you can only provide TRUE or FALSE, you are also less
# flexible in choosing the working directory for the code execution.

# Ok. That's more detailed than me...

# 4. Write a function that opens a graphics device, runs the
# supplied code, and closes the graphics device (always, regardless
# of whether or not the plotting code works).
getOption("device")
#grDevices::dev.new()
?dev.new
#dev.cur()

func_that_opens_device_and_closes_it <- function(x, y = x, sleep = 7 ...) {
if (!is.numeric(x)) {
  warning("\nHa ha!\nYour code didn't work:)!\nI just did 1:5 plot instead... sucker!")
  plot(1:5)
} else {
  message("\nYou're doing fine, my dear padawan!\nHugs and kisses!")
plot(x, y, ...)
}
Sys.sleep(sleep)
on.exit(grDevices::graphics.off(), add = TRUE)
}

func_that_opens_device_and_closes_it("dfdf", sleep = 3)
func_that_opens_device_and_closes_it(1:100, sleep = 3) # Hm...

# 5. We can use on.exit() to implement a simple version of
# capture.output().
capture.output
?capture.output
getwd()
capture.output2 <- function(code) {
  setwd("C:/Users/gauteskr/Advanced_R_exercises")
  temp <- tempfile()
  on.exit(unlink(temp), add = TRUE, after = TRUE)
  sink(temp)
  on.exit(sink(), add = TRUE, after = TRUE)
  force(code)
  on.exit(unlink(temp), add = TRUE, after = TRUE)
  readLines(temp)
}
?force
force(c(1+1, 2+2))
?sink
?tempfile
?tempdir
tempfile(tmpdir = getwd())

capture.output2(cat("a", "b", "c", sep = "\n"))
capture.output(cat("a" , "b", "c", sep = "\n")) # same...


# Compare capture.output() to capture.output2(). How do the
# functions differ? What features have I removed to make the key
# ideas easier to see? How have I rewritten the key ideas so they’re
# easier to understand?

?capture.output()

require(stats)
glmout <- capture.output(summary(glm(case ~ spontaneous+induced,
                                     data = infert, family = binomial())))
glmout[1:5]
capture.output(1+1, 2+2)
capture.output({1+1; 2+2})
capture.output
glmout <- capture.output2(summary(glm(case ~ spontaneous+induced,
                                     data = infert, family = binomial())))
glmout[1:5]
capture.output2(1+1, 2+2)
capture.output2({1+1; 2+2})
force(1, 2+4)

# Wickhams function, due to force(), can't do two arguments at once.
# Also, it doesn't return output. However, it hghlights how sink()
# is used to capture stuff, put it into a file.

# Wickham kind of say "you put these three into a temp-file, and then
# read it back in, before deleting the tempfile". That's
# pedagogical!

# 6.8 Function forms ------------------------------------------------------

# To understand computations in R, two slogans are helpful:

# Everything that exists is an object.
# Everything that happens is a function call.
# — John Chambers

# While everything that happens in R is a result of a function call,
# not all calls look the same. Function calls come in four
# varieties:

# prefix: the function name comes before its arguments, like
# foofy(a, b, c). These constitute of the majority of function
# calls in R.

# infix: the function name comes in between its arguments, like
# x + y. Infix forms are used for many mathematical operators, and
# for user-defined functions that begin and end with %.

# replacement: functions that replace values by assignment, like
# names(df) <- c("a", "b", "c"). They actually look like prefix
# functions.

# special: functions like [[, if, and for. While they don’t have a
# consistent structure, they play important roles in R’s syntax.

# While there are four forms, you actually only need one because
# any call can be written in prefix form.

# 6.8.1 Rewriting to prefix form ------------------------------------------

# An interesting property of R is that every infix, replacement, or
# special form can be rewritten in prefix form. Doing so is useful
# because it helps you better understand the structure of the
# language, it gives you the real name of every function, and it
# allows you to modify those functions for fun and profit.

# The following example shows three pairs of equivalent calls,
# rewriting an infix form, replacement form, and a special form
# into prefix form.

x + y
`+`(x, y)

names(df) <- c("x", "y", "z")
`names<-`(df, c("x", "y", "z"))

for (i in 1:10) print(i)
`for`(i, 1:10, print(i))

# Suprisingly, in R, for can be called like a regular function! The
# same is true for basically every operation in R, which means that
# knowing the function name of a non-prefix function allows you to
# override its behaviour. For example, if you’re ever feeling
# particularly evil, run the following code while a friend is away
# from their computer. It will introduce a fun bug: 10% of the
# time, it will add 1 to any numeric calculation inside the
# parentheses.

`(` <- function(e1) {
  if (is.numeric(e1) && runif(1) < 0.1) {
    e1 + 1
  } else {
    e1
  }
}

replicate(50, (1+2))

rm(`(`)

# Of course, overriding built-in functions like this is a bad idea,
# but, as you’ll learn in Section 21.2.5, it’s possible to apply it
# only to selected code blocks. This provides a clean and elegant
# approach to writing domain specific languages and translators to
# other languages.

# A more useful application comes up when using functional
# programming tools. For example, you could use lapply() to add 3
# to every element of a list by first defining a function add():

add <- function(x, y) x + y
lapply(list(1:4, 4:5), add, 3)

# But we can also get the same result simply by relying on the
# existing + function:

lapply(list(1:4, 4:5), `+`, 3)

# We’ll explore this idea in detail in Section 9.

# 6.8.2 Prefix form -------------------------------------------------------

# The prefix form is the most common form in R code, and indeed in
# the majority of programming languages. Prefix calls in R are a
# little special because you can specify arguments in three ways:

# By position, like help(mean).
# Using partial matching, like help(top = mean).
# By name, like help(topic = mean).
# As illustrated by the following chunk, arguments are matched by
# exact name, then with unique prefixes, and finally by position.

k01 <- function(abcdef, bcde1, bcde2) {
  list(a = abcdef, b1 = bcde1, b2 = bcde2)
}

str(k01(1, 2, 3))
str(k01(2, 3, abcdef = 1))
# Can abbreviate long argument names:
str(k01(2, 3, a = 1))
# But this doesn't work because abbreviation is ambiguous
str(k01(1, 3, b = 1)) #ERROR

# In general, use positional matching only for the first one or two
# arguments; they will be the most commonly used, and most readers
# will know what they are. Avoid using positional matching for less
# commonly used arguments, and never use partial matching.
# Unfortunately you can’t disable partial matching, but you can
# turn it into a warning with the warnPartialMatchArgs option:

options(warnPartialMatchArgs = TRUE)
str(k01(a = 1, 2, 3))

# 6.8.3 Infix functions ---------------------------------------------------

# Infix functions get their name from the fact the function name
# comes in between its arguments, and hence have two arguments. R
# comes with a number of built-in infix operators: :, ::, :::, $,
# @, ^, *, /, +, -, >, >=, <, <=, ==, !=, !, &, &&, |, ||, ~, <-,
# and <<-. You can also create your own infix functions that start
# and end with %. Base R uses this pattern to define
# %%, %*%, %/%, %in%, %o%, and %x%.

# Defining your own infix function is simple. You create a two
# argument function and bind it to a name that starts and ends with
# %:

`%+%` <- function(x, y) paste(x, y)
"new" %+% "string"

# The names of infix functions are more flexible than regular R
# functions: they can contain any sequence of characters except
# for %. You will need to escape any special characters in the
# string used to define the function, but not when you call it:

`% %` <- function(a, b) paste(a, b)
`%/\\%` <- function(a, b) paste(a, b)
"a" % % "b"
"a" %/\% "b"

# R’s default precedence rules mean that infix operators are
# composed left to right:

`%-%` <- function(a, b) paste("(", a, " %-% ", b, ")")
"a" %-% "b" %-% "c"

# There are two special infix functions that can be called with a
# single argument: + and -.

-1
+10

# 6.8.4 Replacement functions ---------------------------------------------

# Replacement functions act like they modify their arguments in
# place, and have the special name xxx<-. They must have arguments
# named x and value, and must return the modified object. For
# example, the following function modifies the second element of a
# vector:

`second<-` <- function(x, value) {
  x[2] <- value
  x
}

# Replacement functions are used by placing the function call on
# the left side of <-:

x <- 1:10
second(x) <- 5L
x

# I say they act like they modify their arguments in place, because,
# as explained in Section 2.5, they actually create a modified copy.
# We can see that by using tracemem():

x <- 1:10
tracemem(x)
second(x) <- 6L

# If your replacement function needs additional arguments, place
# them between x and value, and call the replacement function with
# additional arguments on the left:
`modify<-` <- function(x, position, value) {
  x[position] <- value
  x
}
modify(x, 1) <- 10
x

# When you write modify(x, 1) <- 10, behind the scenes R turns it
# into:

x <- `modify<-`(x, 1, 10)

# Combining replacement with other functions requires more complex
# translation. For example:

x <- c(a = 1, b = 2, c = 3)
names(x)
names(x)[2] <- "two"
names(x)

# is translated into:
`*tmp*` <- x
x <- `names<-`(`*tmp*`, `[<-`(names(`*tmp*`), 2, "two"))
rm(`*tmp*`)

# (Yes, it really does create a local variable named *tmp*, which
# is removed afterwards.)

# 6.8.5 Special forms -----------------------------------------------------

# Finally, there are a bunch of language features that are usually
# written in special ways, but also have prefix forms. These include
# parentheses:

# (x) (`(`(x))
# {x} (`{`(x)).

# The subsetting operators:

#  x[i] (`[`(x, i))
#  x[[i]] (`[[`(x, i))

# And the tools of control flow:

# if (cond) true (`if`(cond, true))
# if (cond) true else false (`if`(cond, true, false))
# for(var in seq) action (`for`(var, seq, action))
# while(cond) action (`while`(cond, action))
# repeat expr (`repeat`(expr))
# next (`next`())
# break (`break`())

# Finally, the most complex is the function function:

# function(arg1, arg2) {body}
# (`function`(alist(arg1, arg2), body, env))

# Knowing the name of the function that underlies a special form is
# useful for getting documentation: ?( is a syntax error; ?`(` will
# give you the documentation for parentheses.

# All special forms are implemented as primitive functions (i.e. in
# C); this means printing these functions is not informative:

`for`

# 6.8.6 Exercises ---------------------------------------------------------

# 1. Rewrite the following code snippets into prefix form:

1 + 2 + 3
1 + (2 + 3)
if (length(x) <= 5) x[[5]] else x[[n]]

# Answers:
`+`(`+`(1, 2), 3)
`+`(1, `+`(2, 3))
`if`(length(x) <= 5, x[[5]], x[[n]])
`

# 2. Clarify the following list of odd function calls:

x <- sample(replace = TRUE, 20, x = c(1:10, NA))
y <- runif(min = 0, max = 1, 20)
cor(m = "k", y = y, u = "p", x = x)

# Answers:
x <- sample(c(1:10, NA), size = 20, replace = TRUE)
y <- runif(20, min = 0, max = 1)
cor(x, y, use = "pairwise.complete.obs", method = "kendall")

# 3. Explain why the following code fails:

modify(get("x"), 1) <- 10
x <- 1:10
modify(get("x"), 1) <- 10
y <- get("x")
`modify<-`
x <- 1:10
tracemem(x)
y <- get("x")
z <- y
z[1] <- 4
z <- x
tracemem(x)
tracemem(get("x"))
get("x")[3] <- 10
get
z[2] <- 5
??modify
get("%o%")
outer

# Answer:
# It's the get()-function that doesn't work:
get("x")[3] <- 10
# Error in get("x")[3] <- 10 :
# target of assignment expands to non-language object
# Since I don't know what this means, I will consult the solutions:

# "R internally transforms the code, and the transformed code
# reproduces the error above:
get
get("x") <- `modify<-`(get("x"), 1, 10)
#> Error in get("x") <- `modify<-`(get("x"), 1, 10) :
#>   target of assignment expands to non-language object
# The error occurs during the assignment because no corresponding
# replacement function, i.e. get<-, exists for get()."

# Let's test this...

`get<-` <- function(xchar, x, pos = -1L, envir = as.environment(pos), mode = "any",
                    inherits = TRUE) {
  x <- .Internal(get(xchar, envir, mode, inherits))
  return(x)
  }
get <- `get<-`
modify(`get<-`("x"), 1) <- 10 # Nope...
rm(get)

# 4. Create a replacement function that modifies a random location
# in a vector.

`randmod<-` <- function(x, position = modthis, new) {
modthis <- sample(length(x), 1)
x[modthis] <<- new
}
x
`randmod<-`(x, 11)
rm(randmod)
`randmod<-`(x) <- 11
randmod(x) <- 11
names(x) <- c("a", "b")

# 5. Write your own version of + that pastes its inputs together if
# they are character vectors but behaves as usual otherwise. In
# other words, make this code work:

1 + 2
"a" + "b"

# 6. Create a list of all the replacement functions found in the
# base package. Which ones are primitive functions? (Hint: use
# apropos().)

# 7. What are valid names for user-created infix functions?

# 8. Create an infix xor() operator.

# 9. Create infix versions of the set functions intersect(),
# union(), and setdiff(). You might call them %n%, %u%, and %/% to
# match conventions from mathematics.

# QUIZ --------------------------------------------------------------------

# Answer the following questions to see if you can safely skip this chapter. You can find the answers in Section 6.9.

# What are the three components of a function?

# What does the following code return?

x <- 10
f1 <- function(x) {
  function() {
    x + 10
  }
}
f1(1)()

# How would you usually write this code?

`+`(1, `*`(2, 3))

# How could you make this call easier to read?

mean(, TRUE, x = c(1:10, NA))

# Does the following code throw an error when executed? Why or why not?

f2 <- function(a, b) {
    a * 10
  }
f2(10, stop("This is an error!"))

# What is an infix function? How do you write it? What’s a replacement function? How do you write it?

# How do you ensure that cleanup action occurs regardless of how a function exits?
